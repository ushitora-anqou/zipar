open Zipar
open OUnit2

let command ?(stdout = "/dev/null") cmd args =
  Sys.command (Filename.quote_command ~stdout cmd args)

let read_regular_file_size file_path =
  let fd = Unix.openfile file_path [ O_RDONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  Unix.lseek fd 0 SEEK_END

let assert_equal_string ~ctxt =
  assert_equal ~ctxt ~cmp:String.equal ~printer:Fun.id

let assert_equal_int ~ctxt =
  assert_equal ~ctxt ~cmp:Int.equal ~printer:Int.to_string

let assert_false ~ctxt =
  assert_equal ~ctxt ~cmp:Bool.equal ~printer:Bool.to_string false

let assert_true ~ctxt =
  assert_equal ~ctxt ~cmp:Bool.equal ~printer:Bool.to_string true

let test_list_entries ctxt =
  with_bracket_chdir ctxt "../../../test/testdata/testdir1" (fun ctxt ->
      let es = Cmd.list_entries "." in
      assert_equal_string ~ctxt "a/" (List.nth es 0).file_path;
      assert_equal_int ~ctxt 0 (List.nth es 0).size;
      assert_equal_string ~ctxt "a/b" (List.nth es 1).file_path;
      assert_equal_int ~ctxt 30 (List.nth es 1).size;
      assert_equal_string ~ctxt "c" (List.nth es 2).file_path;
      assert_equal_int ~ctxt 30 (List.nth es 2).size;

      let es = Cmd.list_entries "a" in
      assert_equal_string ~ctxt "a/" (List.nth es 0).file_path;
      assert_equal_string ~ctxt "a/b" (List.nth es 1).file_path;

      let es = Cmd.list_entries "c" in
      assert_equal_string ~ctxt "c" (List.nth es 0).file_path;

      ());

  with_bracket_chdir ctxt "../../../test/testdata" (fun ctxt ->
      let es = Cmd.list_entries "." in
      assert_equal_string ~ctxt "testdir1/" (List.nth es 0).file_path;
      assert_equal_int ~ctxt 0 (List.nth es 0).size;
      assert_equal_string ~ctxt "testdir1/a/" (List.nth es 1).file_path;
      assert_equal_string ~ctxt "testdir1/a/b" (List.nth es 2).file_path;
      assert_equal_string ~ctxt "testdir1/c" (List.nth es 3).file_path;

      let es = Cmd.list_entries "testdir1" in
      assert_equal_string ~ctxt "testdir1/" (List.nth es 0).file_path;
      assert_equal_string ~ctxt "testdir1/a/" (List.nth es 1).file_path;
      assert_equal_string ~ctxt "testdir1/a/b" (List.nth es 2).file_path;
      assert_equal_string ~ctxt "testdir1/c" (List.nth es 3).file_path;

      ());

  ()

let test_populate_zip_info ctxt =
  with_bracket_chdir ctxt "../../../test/testdata/testdir1" (fun ctxt ->
      let es = Cmd.list_entries "." in
      let total_size, size_of_central_directory, needs_zip64_eocd =
        Cmd.populate_zip_info es
      in
      assert_equal_int ~ctxt 478 total_size;
      assert_equal_int ~ctxt 216 size_of_central_directory;
      assert_false ~ctxt needs_zip64_eocd;
      assert_equal_int ~ctxt 0 (List.nth es 0).zip_local_file_header_offset;
      assert_equal_int ~ctxt 60 (List.nth es 1).zip_local_file_header_offset;
      assert_equal_int ~ctxt 151 (List.nth es 2).zip_local_file_header_offset;
      assert_equal_int ~ctxt 240
        (List.nth es 0).zip_central_directory_entry_offset;
      assert_equal_int ~ctxt 312
        (List.nth es 1).zip_central_directory_entry_offset;
      assert_equal_int ~ctxt 385
        (List.nth es 2).zip_central_directory_entry_offset;
      ());

  with_bracket_chdir ctxt "../../../test/testdata/testdir2" (fun ctxt ->
      let es = Cmd.list_entries "." in
      let total_size, size_of_central_directory, needs_zip64_eocd =
        Cmd.populate_zip_info es
      in
      assert_equal_int ~ctxt 5368709730 total_size;
      assert_equal_int ~ctxt 250 size_of_central_directory;
      assert_true ~ctxt needs_zip64_eocd;
      ());
  ()

let test_write_zip ctxt =
  let e2e ?(expected_rsync_stdout = ".d..t...... ./") wd expected_size =
    with_bracket_chdir ctxt wd @@ fun ctxt ->
    let temp_dir = bracket_tmpdir ~prefix:"zipar-test-" ctxt in
    let output_path = Filename.concat temp_dir "zip" in
    let output_unzip_path = Filename.concat temp_dir "unzip" in
    let output_rsync_path = Filename.concat temp_dir "rsync" in

    (* Zip everything in the current directory *)
    let input_paths = [ "." ] in
    Cmd.(
      with_domainslib_pool (fun pool -> write_zip pool output_path input_paths));

    (* Make sure the zip file size is correct *)
    assert_equal_int ~ctxt expected_size (read_regular_file_size output_path);

    (* Unzip the archive we made *)
    let unzip_res = command "unzip" [ output_path; "-d"; output_unzip_path ] in
    assert_equal_int ~ctxt 0 unzip_res;

    (* Make sure the unzipped files are the same as the original ones *)
    let rsync_res =
      command ~stdout:output_rsync_path "rsync"
        [ "-acin"; "--delete"; wd ^ "/"; output_unzip_path ^ "/" ]
    in
    assert_equal_int ~ctxt 0 rsync_res;
    let ic = open_in_bin output_rsync_path in
    Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
    let rsync_stdout = In_channel.input_all ic in
    assert_equal_string ~ctxt
      (String.trim expected_rsync_stdout)
      (String.trim rsync_stdout);
    ()
  in
  e2e "../../../test/testdata/testdir5" 450
    ~expected_rsync_stdout:
      {|
.d..t...... ./
.L..t...... b -> a
.L..t...... c -> /bin/sh|};
  e2e "../../../test/testdata/testdir1" 478;
  e2e "../../../test/testdata/testdir3" 9021640;
  e2e "../../../test/testdata/testdir4" 9021854;
  e2e "../../../test/testdata/testdir2" 5368709730;
  ()

let () =
  run_test_tt_main
    ("zipar"
    >::: [
           "list_entries" >:: test_list_entries;
           "populate_zip_info" >:: test_populate_zip_info;
           "write_zip" >:: test_write_zip;
         ])
