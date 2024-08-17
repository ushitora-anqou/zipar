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

let fits_in_2B x = x <= 0xffff
let fits_in_4B x = x <= 0xffffffff
let clamp_2B x = min x 0xffff
let clamp_4B x = min x 0xffffffff

type entry = {
  file_path : string;
  stat : Unix.stats;
  size : int; [@default 0]
  symlink : string option;
      (* symlink should be set only if stat.st_kind = S_LNK *)
  mutable zip_local_file_header_offset : int; [@default 0]
  mutable zip_central_directory_entry_offset : int; [@default 0]
}
[@@deriving make]

let read_regular_file_size file_path =
  let fd = Unix.openfile file_path [ O_RDONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  Unix.lseek fd 0 SEEK_END

let make_entry_for_reg stat file_path =
  let size = read_regular_file_size file_path in
  make_entry ~file_path ~stat ~size ()

let make_entry_for_lnk stat file_path =
  let symlink = Unix.readlink file_path in
  make_entry ~file_path ~stat ~size:(String.length symlink) ~symlink ()

let list_entries file_path =
  let stat = Unix.lstat file_path in
  match stat.st_kind with
  | S_REG -> [ make_entry_for_reg stat file_path ]
  | S_LNK -> [ make_entry_for_lnk stat file_path ]
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
          | _, S_REG -> entries := make_entry_for_reg stat file_path :: !entries
          | _, S_LNK -> entries := make_entry_for_lnk stat file_path :: !entries
          | _, S_DIR ->
              entries :=
                make_entry ~file_path:(file_path ^ "/") ~stat () :: !entries
          | _ -> ());
      !entries
      |> List.sort (fun lhs rhs -> String.compare lhs.file_path rhs.file_path)
  | _ ->
      Printf.eprintf "list_entries: ignoring %s\n" file_path;
      []

let populate_zip_info entries =
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
         let zip64_extra_field_size = if fits_in_4B entry.size then 0 else 20 in
         offset :=
           !offset + 30 + file_name_size + extra_field_size
           + zip64_extra_field_size + entry.size);

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
           24 (* extended timestamp (modtime + actime) + Info-ZIP Unix (new) *)
         in
         let zip64_extra_field_size =
           let x =
             (if fits_in_4B entry.size then 0 else 16)
             + if fits_in_4B entry.zip_local_file_header_offset then 0 else 8
           in
           if x = 0 then 0 else 4 + x
         in
         let file_comment = 0 in
         offset :=
           !offset + 46 + file_name_size + extra_field_size
           + zip64_extra_field_size + file_comment);

  let offset_of_start_of_central_directory =
    (List.hd entries).zip_central_directory_entry_offset
  in
  let size_of_central_directory =
    !offset - offset_of_start_of_central_directory
  in
  let needs_zip64_eocd =
    not
      (fits_in_2B (List.length entries)
      && fits_in_4B size_of_central_directory
      && fits_in_4B offset_of_start_of_central_directory)
  in

  (* Zip64 end of central directory record

       https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT

     4.3.14  Zip64 end of central directory record

          zip64 end of central dir
          signature                       4 bytes  (0x06064b50)
          size of zip64 end of central
          directory record                8 bytes
          version made by                 2 bytes
          version needed to extract       2 bytes
          number of this disk             4 bytes
          number of the disk with the
          start of the central directory  4 bytes
          total number of entries in the
          central directory on this disk  8 bytes
          total number of entries in the
          central directory               8 bytes
          size of the central directory   8 bytes
          offset of start of central
          directory with respect to
          the starting disk number        8 bytes
          zip64 extensible data sector    (variable size)

     4.3.15 Zip64 end of central directory locator

        zip64 end of central dir locator
        signature                       4 bytes  (0x07064b50)
        number of the disk with the
        start of the zip64 end of
        central directory               4 bytes
        relative offset of the zip64
        end of central directory record 8 bytes
        total number of disks           4 bytes
  *)
  if needs_zip64_eocd then offset := !offset + 76;

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

  (!offset, size_of_central_directory, needs_zip64_eocd)

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

let write8 buf off v = Bigstringaf.set buf off v
let write16le buf off v = Bigstringaf.set_int16_le buf off v
let write32le buf off v = Bigstringaf.set_int32_le buf off v
let write64le buf off v = Bigstringaf.set_int64_le buf off v

let blit src ~src_off dst ~dst_off ~len =
  Bigstringaf.blit src ~src_off dst ~dst_off ~len

let blit_from_string src ~src_off dst ~dst_off ~len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

let write_last_mod_file_time_date buf off (mtime : Unix.tm) =
  write16le buf off
    ((mtime.tm_hour lsl 11) lor (mtime.tm_min lsl 5)
    lor ((if mtime.tm_sec mod 2 = 0 then 0 else 1) + (mtime.tm_sec / 2)));
  write16le buf (off + 2)
    (((mtime.tm_year - 80) lsl 9)
    lor ((mtime.tm_mon + 1) lsl 5)
    lor mtime.tm_mday);
  off + 4

let write_crc32 buf entry =
  let checksum =
    match entry.stat.st_kind with
    | S_DIR -> 0l
    | S_REG ->
        with_input_buffer entry.file_path (fun ibuf ->
            Checkseum.Crc32.(digest_bigstring ibuf 0 entry.size default)
            |> Optint.to_int32)
    | S_LNK ->
        Checkseum.Crc32.(
          digest_string (Option.get entry.symlink) 0 entry.size default)
        |> Optint.to_int32
    | _ -> assert false
  in
  write32le buf (entry.zip_local_file_header_offset + 14) checksum;
  write32le buf (entry.zip_central_directory_entry_offset + 16) checksum;
  ()

let write_string buf off s =
  let len = String.length s in
  blit_from_string s ~src_off:0 buf ~dst_off:off ~len;
  off + len

let write_local_file_header buf entry =
  let off = ref entry.zip_local_file_header_offset in
  let needs_zip64_size = not (fits_in_4B entry.size) in

  (* local file header signature     4 bytes  (0x04034b50) *)
  write32le buf !off 0x04034b50l;
  off := !off + 4;

  (* version needed to extract       2 bytes *)
  write16le buf !off (if needs_zip64_size then 0x002d else 0x000a);
  off := !off + 2;

  (* general purpose bit flag        2 bytes *)
  write16le buf !off 0x0000;
  off := !off + 2;

  (* compression method              2 bytes *)
  write16le buf !off 0x0000;
  off := !off + 2;

  (* last mod file time              2 bytes
     last mod file date              2 bytes *)
  off :=
    write_last_mod_file_time_date buf !off (Unix.localtime entry.stat.st_mtime);

  (* crc-32                          4 bytes
     This field should be populated by another domain *)
  off := !off + 4;

  (* compressed size                 4 bytes
     uncompressed size               4 bytes *)
  let uncompressed_size = clamp_4B entry.size in
  write32le buf !off (Int32.of_int uncompressed_size);
  write32le buf (!off + 4) (Int32.of_int uncompressed_size);
  off := !off + 8;

  (* file name length                2 bytes *)
  write16le buf !off (String.length entry.file_path);
  off := !off + 2;

  (* extra field length              2 bytes *)
  write16le buf !off (if needs_zip64_size then 0x0030 else 0x001c);
  off := !off + 2;

  (* file name (variable size) *)
  off := write_string buf !off entry.file_path;

  (* extra field (variable size) *)
  (* extra field: extended timestamp (local-header version) *)
  write16le buf !off 0x5455;
  write16le buf (!off + 2) 0x0009;
  write8 buf (!off + 4) (char_of_int 0x03);
  write32le buf (!off + 5) (Int32.of_float entry.stat.st_mtime);
  write32le buf (!off + 9) (Int32.of_float entry.stat.st_atime);
  off := !off + 13;

  (* extra field: Info-ZIP Unix (new) *)
  write16le buf !off 0x7875;
  write16le buf (!off + 2) 0x000b;
  write8 buf (!off + 4) (char_of_int 0x01);
  write8 buf (!off + 5) (char_of_int 0x04);
  write32le buf (!off + 6) (Int32.of_int entry.stat.st_uid);
  write8 buf (!off + 10) (char_of_int 0x04);
  write32le buf (!off + 11) (Int32.of_int entry.stat.st_gid);
  off := !off + 15;

  (* extra field: Zip64 extended information extra field *)
  if needs_zip64_size then (
    write16le buf !off 0x0001;
    write16le buf (!off + 2) 0x0010;
    write64le buf (!off + 4) (Int64.of_int entry.size);
    write64le buf (!off + 12) (Int64.of_int entry.size);
    off := !off + 20);

  (* file data *)
  (match entry.stat.st_kind with
  | S_DIR -> ()
  | S_REG ->
      with_input_buffer entry.file_path (fun ibuf ->
          blit ibuf ~src_off:0 buf ~dst_off:!off ~len:entry.size)
  | S_LNK -> off := write_string buf !off (Option.get entry.symlink)
  | _ -> assert false);
  ()

let write_central_directory_entry buf entry =
  let off = ref entry.zip_central_directory_entry_offset in
  let needs_zip64_size = not (fits_in_4B entry.size) in
  let needs_zip64_offset =
    not (fits_in_4B entry.zip_local_file_header_offset)
  in
  let needs_zip64 = needs_zip64_size || needs_zip64_offset in

  (* central file header signature   4 bytes  (0x02014b50) *)
  write32le buf !off 0x02014b50l;
  off := !off + 4;

  (* version made by                 2 bytes *)
  write16le buf !off 0x031e;
  off := !off + 2;

  (* version needed to extract       2 bytes *)
  write16le buf !off (if needs_zip64_size then 0x002d else 0x000a);
  off := !off + 2;

  (* general purpose bit flag        2 bytes *)
  write16le buf !off 0x0000;
  off := !off + 2;

  (* compression method              2 bytes *)
  write16le buf !off 0x0000;
  off := !off + 2;

  (* last mod file time              2 bytes
     last mod file date              2 bytes *)
  off :=
    write_last_mod_file_time_date buf !off (Unix.localtime entry.stat.st_mtime);

  (* crc-32                          4 bytes
     This field should be populated by another domain *)
  off := !off + 4;

  (* compressed size                 4 bytes
     uncompressed size               4 bytes *)
  let uncompressed_size = clamp_4B entry.size in
  write32le buf !off (Int32.of_int uncompressed_size);
  write32le buf (!off + 4) (Int32.of_int uncompressed_size);
  off := !off + 8;

  (* file name length                2 bytes *)
  write16le buf !off (String.length entry.file_path);
  off := !off + 2;

  (* extra field length              2 bytes *)
  write16le buf !off
    (24
    + (if needs_zip64 then 4 else 0)
    + (if needs_zip64_size then 16 else 0)
    + if needs_zip64_offset then 8 else 0);
  off := !off + 2;

  (* file comment length             2 bytes *)
  write16le buf !off 0x0000;
  off := !off + 2;

  (* disk number start               2 bytes *)
  write16le buf !off 0x0000;
  off := !off + 2;

  (* internal file attributes        2 bytes *)
  write16le buf !off 0x0000;
  off := !off + 2;

  (* external file attributes        4 bytes
     Thanks to: https://unix.stackexchange.com/a/14727 *)
  let unix_attr =
    (* TTTTsstrwxrwxrwx *)
    entry.stat.st_perm land 0o7777
    lor
    match entry.stat.st_kind with
    | S_REG -> 0o100000
    | S_DIR -> 0o040000
    | S_LNK -> 0o120000
    | S_CHR | S_BLK | S_FIFO | S_SOCK -> assert false
  in
  let dos_attr =
    (* ADVSHR *)
    match entry.stat.st_kind with
    | S_REG | S_LNK -> 0o00
    | S_DIR -> 0o20
    | _ -> assert false
  in
  write32le buf !off (Int32.of_int ((unix_attr lsl 16) lor dos_attr));
  off := !off + 4;

  (* relative offset of local header 4 bytes *)
  let relative_offset_of_local_header =
    clamp_4B entry.zip_local_file_header_offset
  in
  write32le buf !off (Int32.of_int relative_offset_of_local_header);
  off := !off + 4;

  (* file name (variable size) *)
  off := write_string buf !off entry.file_path;

  (* extra field (variable size) *)
  (* extra field: extended timestamp (local-header version) *)
  write16le buf !off 0x5455;
  write16le buf (!off + 2) 0x0005;
  write8 buf (!off + 4) (char_of_int 0x03);
  write32le buf (!off + 5) (Int32.of_float entry.stat.st_mtime);
  off := !off + 9;

  (* extra field: Info-ZIP Unix (new) *)
  write16le buf !off 0x7875;
  write16le buf (!off + 2) 0x000b;
  write8 buf (!off + 4) (char_of_int 0x01);
  write8 buf (!off + 5) (char_of_int 0x04);
  write32le buf (!off + 6) (Int32.of_int entry.stat.st_uid);
  write8 buf (!off + 10) (char_of_int 0x04);
  write32le buf (!off + 11) (Int32.of_int entry.stat.st_gid);
  off := !off + 15;

  (* extra field: Zip64 extended information extra field *)
  if needs_zip64 then (
    write16le buf !off 0x0001;
    off := !off + 2;
    let size_off = !off in
    off := !off + 2;
    if needs_zip64_size then (
      write64le buf !off (Int64.of_int entry.size);
      write64le buf (!off + 8) (Int64.of_int entry.size);
      off := !off + 16);
    if needs_zip64_offset then (
      write64le buf !off (Int64.of_int entry.zip_local_file_header_offset);
      off := !off + 8);
    write16le buf size_off (!off - size_off - 2));

  (* file comment (variable size)
     (nothing) *)
  ()

let write_eocd buf off entries size_of_central_directory =
  (* end of central dir signature    4 bytes  (0x06054b50) *)
  write32le buf off 0x06054b50l;
  let off = off + 4 in

  (* number of this disk             2 bytes *)
  write16le buf off 0x0000;
  let off = off + 2 in

  (* number of the disk with the
     start of the central directory  2 bytes *)
  write16le buf off 0x0000;
  let off = off + 2 in

  (* total number of entries in the
     central directory on this disk  2 bytes
     total number of entries in
     the central directory           2 bytes *)
  let total_number_of_entries = clamp_2B (List.length entries) in
  write16le buf off total_number_of_entries;
  write16le buf (off + 2) total_number_of_entries;
  let off = off + 4 in

  (* size of the central directory   4 bytes *)
  write32le buf off (Int32.of_int (clamp_4B size_of_central_directory));
  let off = off + 4 in

  (* offset of start of central
     directory with respect to
     the starting disk number        4 bytes *)
  let offset_of_start_of_central_directory =
    clamp_4B (List.hd entries).zip_central_directory_entry_offset
  in
  write32le buf off (Int32.of_int offset_of_start_of_central_directory);
  let off = off + 4 in

  (* .ZIP file comment length        2 bytes *)
  write16le buf off 0x0000;
  let _off = off + 2 in

  (* .ZIP file comment       (variable size)
     (nothing) *)
  ()

let write_zip64_eocd buf off entries size_of_central_directory =
  let start_off = off in

  (* Zip64 end of central directory record *)
  (* zip64 end of central dir
     signature                       4 bytes  (0x06064b50) *)
  write32le buf off 0x06064b50l;
  let off = off + 4 in

  (* size of zip64 end of central
     directory record                8 bytes *)
  write64le buf off 0x2cL;
  let off = off + 8 in

  (* version made by                 2 bytes *)
  write16le buf off 0x031e;
  let off = off + 2 in

  (* version needed to extract       2 bytes *)
  write16le buf off 0x002d;
  let off = off + 2 in

  (* number of this disk             4 bytes *)
  write32le buf off 0x00000000l;
  let off = off + 4 in

  (* number of the disk with the
     start of the central directory  4 bytes *)
  write32le buf off 0x00000000l;
  let off = off + 4 in

  (* total number of entries in the
     central directory on this disk  8 bytes *)
  write64le buf off (Int64.of_int (List.length entries));
  let off = off + 8 in

  (*
     total number of entries in the
     central directory               8 bytes *)
  write64le buf off (Int64.of_int (List.length entries));
  let off = off + 8 in

  (* size of the central directory   8 bytes *)
  write64le buf off (Int64.of_int size_of_central_directory);
  let off = off + 8 in

  (* offset of start of central
     directory with respect to
     the starting disk number        8 bytes *)
  write64le buf off
    (Int64.of_int (List.hd entries).zip_central_directory_entry_offset);
  let off = off + 8 in

  (* zip64 extensible data sector    (variable size)
     (nothing) *)

  (* Zip64 end of central directory locator *)
  (* zip64 end of central dir locator
     signature                       4 bytes  (0x07064b50) *)
  write32le buf off 0x07064b50l;
  let off = off + 4 in

  (* number of the disk with the
     start of the zip64 end of
     central directory               4 bytes *)
  write32le buf off 0x00000000l;
  let off = off + 4 in

  (* relative offset of the zip64
     end of central directory record 8 bytes *)
  write64le buf off (Int64.of_int start_off);
  let off = off + 8 in

  (* total number of disks           4 bytes *)
  write32le buf off 0x00000001l;
  let _off = off + 4 in

  ()

let write_zip pool output_path input_paths =
  let entries = input_paths |> List.map list_entries |> List.flatten in
  let total_size, size_of_central_directory, needs_zip64_eocd =
    populate_zip_info entries
  in
  let num_entries = List.length entries in
  with_output_buffer output_path total_size @@ fun (buf : memory_mapped_file) ->
  Domainslib.Task.parallel_for ~start:0 ~finish:(num_entries * 2)
    ~body:(fun i ->
      if i = num_entries * 2 then (
        if needs_zip64_eocd then
          write_zip64_eocd buf
            (total_size - (22 + 76))
            entries size_of_central_directory;
        write_eocd buf (total_size - 22) entries size_of_central_directory)
      else if i mod 2 = 0 then
        let entry = List.nth entries (i / 2) in
        write_crc32 buf entry
      else
        let entry = List.nth entries (i / 2) in
        write_local_file_header buf entry;
        write_central_directory_entry buf entry)
    pool;
  ()

let with_domainslib_pool ?(num_domains = Domain.recommended_domain_count ()) f =
  let pool = Domainslib.Task.setup_pool ~num_domains () in
  Fun.protect ~finally:(fun () -> Domainslib.Task.teardown_pool pool)
  @@ fun () -> Domainslib.Task.run pool (fun () -> f pool)

let run zipfile paths =
  with_domainslib_pool @@ fun pool ->
  write_zip pool zipfile paths;
  ()
