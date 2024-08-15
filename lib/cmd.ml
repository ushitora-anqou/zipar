let rec walk dirname f =
  let h = Unix.opendir dirname in
  Fun.protect ~finally:(fun () -> Unix.closedir h) @@ fun () ->
  let rec loop () =
    match Unix.readdir h with
    | exception End_of_file -> ()
    | filename ->
        let file_path = Filename.concat dirname filename in
        let stat = Unix.lstat file_path in
        f dirname filename file_path stat;
        (match stat.st_kind with
        | S_DIR when filename <> "." && filename <> ".." -> walk file_path f
        | _ -> ());
        loop ()
  in
  loop ()

type entry = {
  file_path : string;
  stat : Unix.stats;
  size : int; [@default 0]
  mutable zip_local_file_header_offset : int; [@default 0]
  mutable zip_central_directory_entry_offset : int; [@default 0]
}
[@@deriving make]

let read_regular_file_size file_path =
  let fd = Unix.openfile file_path [ O_RDONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  Unix.lseek fd 0 SEEK_END

let list_entries file_path =
  let stat = Unix.lstat file_path in
  match stat.st_kind with
  | S_REG ->
      let size = read_regular_file_size file_path in
      [ make_entry ~file_path ~stat ~size () ]
  | S_DIR ->
      let entries = ref [] in

      (* Register the root path as an entry unless it's '.' *)
      if file_path <> "." then
        entries :=
          make_entry
            ~file_path:
              (if String.ends_with ~suffix:"/" file_path then file_path
               else file_path ^ "/")
            ~stat ()
          :: !entries;

      walk file_path (fun _dirname filename file_path stat ->
          let file_path =
            (* Remove the unnecessary prefix './' *)
            if String.starts_with ~prefix:"./" file_path then
              String.sub file_path 2 (String.length file_path - 2)
            else file_path
          in
          match (filename, stat.st_kind) with
          | ("." | ".."), _ -> ()
          | _, S_REG ->
              let size = read_regular_file_size file_path in
              entries := make_entry ~file_path ~stat ~size () :: !entries
          | _, S_DIR ->
              entries :=
                make_entry ~file_path:(file_path ^ "/") ~stat () :: !entries
          | _ -> ());
      List.rev !entries
  | _ -> assert false

let populate_zip_offsets entries =
  let offset = ref 0 in

  (* Local file headers *)
  entries
  |> List.iter (fun entry ->
         entry.zip_local_file_header_offset <- !offset;
         (*
            https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT

            4.3.7  Local file header:

               local file header signature     4 bytes  (0x04034b50)
               version needed to extract       2 bytes
               general purpose bit flag        2 bytes
               compression method              2 bytes
               last mod file time              2 bytes
               last mod file date              2 bytes
               crc-32                          4 bytes
               compressed size                 4 bytes
               uncompressed size               4 bytes
               file name length                2 bytes
               extra field length              2 bytes

               file name (variable size)
               extra field (variable size)
         *)
         let file_name_size = String.length entry.file_path in
         let extra_field_size =
           28 (* exnteded timestamp (modtime + actime) + Info-ZIP Unix (new) *)
         in
         offset := !offset + 30 + file_name_size + extra_field_size + entry.size);

  (* Central directory entries *)
  entries
  |> List.iter (fun entry ->
         entry.zip_central_directory_entry_offset <- !offset;
         (*
            https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT

            4.3.12  Central directory structure:

               File header:

                 central file header signature   4 bytes  (0x02014b50)
                 version made by                 2 bytes
                 version needed to extract       2 bytes
                 general purpose bit flag        2 bytes
                 compression method              2 bytes
                 last mod file time              2 bytes
                 last mod file date              2 bytes
                 crc-32                          4 bytes
                 compressed size                 4 bytes
                 uncompressed size               4 bytes
                 file name length                2 bytes
                 extra field length              2 bytes
                 file comment length             2 bytes
                 disk number start               2 bytes
                 internal file attributes        2 bytes
                 external file attributes        4 bytes
                 relative offset of local header 4 bytes

                 file name (variable size)
                 extra field (variable size)
                 file comment (variable size)
         *)
         let file_name_size = String.length entry.file_path in
         let extra_field_size =
           24 (* exnteded timestamp (modtime + actime) + Info-ZIP Unix (new) *)
         in
         let file_comment = 0 in
         offset :=
           !offset + 46 + file_name_size + extra_field_size + file_comment);

  (* End of central directory record

       https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT

     4.3.16  End of central directory record:

        end of central dir signature    4 bytes  (0x06054b50)
        number of this disk             2 bytes
        number of the disk with the
        start of the central directory  2 bytes
        total number of entries in the
        central directory on this disk  2 bytes
        total number of entries in
        the central directory           2 bytes
        size of the central directory   4 bytes
        offset of start of central
        directory with respect to
        the starting disk number        4 bytes
        .ZIP file comment length        2 bytes
        .ZIP file comment       (variable size)
  *)
  offset := !offset + 22;

  !offset

type memory_mapped_file = Bigstringaf.t

let with_input_buffer file_path f =
  let fd = Unix.openfile file_path [ O_RDONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  let buf = Unix.map_file fd Char C_layout false [| -1 |] in
  let buf = Bigarray.array1_of_genarray buf in
  f buf

let with_output_buffer file_path file_size f =
  let fd = Unix.openfile file_path [ O_RDWR; O_CREAT; O_TRUNC ] 0o640 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  ExtUnix.All.fallocate fd 0 file_size;
  let buf = Unix.map_file fd Char C_layout true [| -1 |] in
  let buf = Bigarray.array1_of_genarray buf in
  f buf

let write_last_mod_file_time_date buf off (mtime : Unix.tm) =
  let open Bigstringaf in
  set_int16_le buf off
    ((mtime.tm_hour lsl 11) lor (mtime.tm_min lsl 5)
    lor ((if mtime.tm_sec mod 2 = 0 then 0 else 1) + (mtime.tm_sec / 2)));
  set_int16_le buf (off + 2)
    (((mtime.tm_year - 80) lsl 9)
    lor ((mtime.tm_mon + 1) lsl 5)
    lor mtime.tm_mday);
  off + 4

let write_crc32 buf off entry =
  let open Bigstringaf in
  let checksum =
    if entry.size > 0 then
      with_input_buffer entry.file_path (fun ibuf ->
          Checkseum.Crc32.(digest_bigstring ibuf 0 entry.size default))
      |> Optint.to_int32
    else 0l
  in
  set_int32_le buf off checksum;
  off + 4

let write_string buf off s =
  let len = String.length s in
  Bigstringaf.blit_from_string s ~src_off:0 buf ~dst_off:off ~len;
  off + len

let write_entry buf entry =
  let open Bigstringaf in
  (* *******************
      Local file header
     ******************* *)
  let off = entry.zip_local_file_header_offset in

  (* local file header signature     4 bytes  (0x04034b50) *)
  set_int32_le buf off 0x04034b50l;
  let off = off + 4 in

  (* version needed to extract       2 bytes *)
  set_int16_le buf off 0x000a;
  let off = off + 2 in

  (* general purpose bit flag        2 bytes *)
  set_int16_le buf off 0x0000;
  let off = off + 2 in

  (* compression method              2 bytes *)
  set_int16_le buf off 0x0000;
  let off = off + 2 in

  (* last mod file time              2 bytes
     last mod file date              2 bytes *)
  let off =
    write_last_mod_file_time_date buf off (Unix.localtime entry.stat.st_mtime)
  in

  (* crc-32                          4 bytes *)
  let off = write_crc32 buf off entry in

  (* compressed size                 4 bytes *)
  set_int32_le buf off (Int32.of_int entry.size);
  let off = off + 4 in

  (* uncompressed size               4 bytes *)
  set_int32_le buf off (Int32.of_int entry.size);
  let off = off + 4 in

  (* file name length                2 bytes *)
  set_int16_le buf off (String.length entry.file_path);
  let off = off + 2 in

  (* extra field length              2 bytes *)
  set_int16_le buf off 0x001c;
  let off = off + 2 in

  (* file name (variable size) *)
  let off = write_string buf off entry.file_path in

  (* extra field (variable size) *)
  (* extra field: extended timestamp (local-header version) *)
  set_int16_le buf off 0x5455;
  set_int16_le buf (off + 2) 0x0009;
  set buf (off + 4) (char_of_int 0x03);
  set_int32_le buf (off + 5) (Int32.of_float entry.stat.st_mtime);
  set_int32_le buf (off + 9) (Int32.of_float entry.stat.st_atime);
  let off = off + 13 in

  (* extra field: Info-ZIP Unix (new) *)
  set_int16_le buf off 0x7875;
  set_int16_le buf (off + 2) 0x000b;
  set buf (off + 4) (char_of_int 0x01);
  set buf (off + 5) (char_of_int 0x04);
  set_int32_le buf (off + 6) (Int32.of_int entry.stat.st_uid);
  set buf (off + 10) (char_of_int 0x04);
  set_int32_le buf (off + 11) (Int32.of_int entry.stat.st_gid);
  let off = off + 15 in

  (* file data *)
  if entry.size > 0 then
    with_input_buffer entry.file_path (fun ibuf ->
        blit ibuf ~src_off:0 buf ~dst_off:off ~len:entry.size);

  (* *************************
      Central directory entry
     ************************* *)
  let off = entry.zip_central_directory_entry_offset in

  (* central file header signature   4 bytes  (0x02014b50) *)
  set_int32_le buf off 0x02014b50l;
  let off = off + 4 in

  (* version made by                 2 bytes *)
  set_int16_le buf off 0x031e;
  let off = off + 2 in

  (* version needed to extract       2 bytes *)
  set_int16_le buf off 0x000a;
  let off = off + 2 in

  (* general purpose bit flag        2 bytes *)
  set_int16_le buf off 0x0000;
  let off = off + 2 in

  (* compression method              2 bytes *)
  set_int16_le buf off 0x0000;
  let off = off + 2 in

  (* last mod file time              2 bytes *)
  (* last mod file date              2 bytes *)
  let off =
    write_last_mod_file_time_date buf off (Unix.localtime entry.stat.st_mtime)
  in

  (* crc-32                          4 bytes *)
  let off = write_crc32 buf off entry in

  (* compressed size                 4 bytes *)
  set_int32_le buf off (Int32.of_int entry.size);
  let off = off + 4 in

  (* uncompressed size               4 bytes *)
  set_int32_le buf off (Int32.of_int entry.size);
  let off = off + 4 in

  (* file name length                2 bytes *)
  set_int16_le buf off (String.length entry.file_path);
  let off = off + 2 in

  (* extra field length              2 bytes *)
  set_int16_le buf off 0x0018;
  let off = off + 2 in

  (* file comment length             2 bytes *)
  set_int16_le buf off 0x0000;
  let off = off + 2 in

  (* disk number start               2 bytes *)
  set_int16_le buf off 0x0000;
  let off = off + 2 in

  (* internal file attributes        2 bytes *)
  set_int16_le buf off 0x0000;
  let off = off + 2 in

  (* external file attributes        4 bytes
     Thanks to: https://unix.stackexchange.com/a/14727 *)
  let unix_attr =
    (* TTTTsstrwxrwxrwx *)
    entry.stat.st_perm land 0o7777
    lor
    match entry.stat.st_kind with
    | S_REG -> 0o100000
    | S_DIR -> 0o040000
    | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK -> assert false
  in
  let dos_attr =
    (* ADVSHR *)
    match entry.stat.st_kind with
    | S_REG -> 0o00
    | S_DIR -> 0o20
    | _ -> assert false
  in
  set_int32_le buf off (Int32.of_int ((unix_attr lsl 16) lor dos_attr));
  let off = off + 4 in

  (* relative offset of local header 4 bytes *)
  set_int32_le buf off (Int32.of_int entry.zip_local_file_header_offset);
  let off = off + 4 in

  (* file name (variable size) *)
  let off = write_string buf off entry.file_path in

  (* extra field (variable size) *)
  (* extra field: extended timestamp (local-header version) *)
  set_int16_le buf off 0x5455;
  set_int16_le buf (off + 2) 0x0005;
  set buf (off + 4) (char_of_int 0x03);
  set_int32_le buf (off + 5) (Int32.of_float entry.stat.st_mtime);
  let off = off + 9 in

  (* extra field: Info-ZIP Unix (new) *)
  set_int16_le buf off 0x7875;
  set_int16_le buf (off + 2) 0x000b;
  set buf (off + 4) (char_of_int 0x01);
  set buf (off + 5) (char_of_int 0x04);
  set_int32_le buf (off + 6) (Int32.of_int entry.stat.st_uid);
  set buf (off + 10) (char_of_int 0x04);
  set_int32_le buf (off + 11) (Int32.of_int entry.stat.st_gid);
  let _off = off + 15 in

  (* file comment (variable size)
     (nothing) *)
  ()

let write_eocd buf off entries =
  let open Bigstringaf in
  let size_of_central_directory =
    off - (List.nth entries 0).zip_central_directory_entry_offset
  in

  (* end of central dir signature    4 bytes  (0x06054b50) *)
  set_int32_le buf off 0x06054b50l;
  let off = off + 4 in

  (* number of this disk             2 bytes *)
  set_int16_le buf off 0x0000;
  let off = off + 2 in

  (* number of the disk with the
     start of the central directory  2 bytes *)
  set_int16_le buf off 0x0000;
  let off = off + 2 in

  (* total number of entries in the
     central directory on this disk  2 bytes *)
  set_int16_le buf off (List.length entries);
  let off = off + 2 in

  (* total number of entries in
     the central directory           2 bytes *)
  set_int16_le buf off (List.length entries);
  let off = off + 2 in

  (* size of the central directory   4 bytes *)
  set_int32_le buf off (Int32.of_int size_of_central_directory);
  let off = off + 4 in

  (* offset of start of central
     directory with respect to
     the starting disk number        4 bytes *)
  set_int32_le buf off
    (Int32.of_int (List.nth entries 0).zip_central_directory_entry_offset);
  let off = off + 4 in

  (* .ZIP file comment length        2 bytes *)
  set_int16_le buf off 0x0000;
  let _off = off + 2 in

  (* .ZIP file comment       (variable size)
     (nothing) *)
  ()

let write_zip output_path input_paths =
  let entries = input_paths |> List.map list_entries |> List.flatten in
  let total_size = populate_zip_offsets entries in
  with_output_buffer output_path total_size @@ fun (buf : memory_mapped_file) ->
  entries |> List.iter (write_entry buf);
  write_eocd buf (total_size - 22) entries;
  ()

let run zipfile paths =
  write_zip zipfile paths;
  ()
