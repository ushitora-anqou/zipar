open Zipar

let with_wd path f =
  let old_wd = Unix.getcwd () in
  Unix.chdir path;
  Fun.protect ~finally:(fun () -> Unix.chdir old_wd) @@ fun () -> f ()

let command ?(stdout = "/dev/null") cmd args =
  Sys.command (Filename.quote_command ~stdout cmd args)

let read_regular_file_size file_path =
  let fd = Unix.openfile file_path [ O_RDONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  Unix.lseek fd 0 SEEK_END

let test_list_entries _text_ctxt =
  with_wd "../../../test/testdata/testdir1" (fun () ->
      let es = Cmd.list_entries "." in
      assert ((List.nth es 0).file_path = "a/");
      assert ((List.nth es 0).size = 0);
      assert ((List.nth es 1).file_path = "a/b");
      assert ((List.nth es 1).size = 30);
      assert ((List.nth es 2).file_path = "c");
      assert ((List.nth es 2).size = 30);

      let es = Cmd.list_entries "a" in
      assert ((List.nth es 0).file_path = "a/");
      assert ((List.nth es 1).file_path = "a/b");

      let es = Cmd.list_entries "c" in
      assert ((List.nth es 0).file_path = "c");

      ());

  with_wd "../../../test/testdata" (fun () ->
      let es = Cmd.list_entries "." in
      assert ((List.nth es 0).file_path = "testdir1/");
      assert ((List.nth es 0).size = 0);
      assert ((List.nth es 1).file_path = "testdir1/a/");
      assert ((List.nth es 2).file_path = "testdir1/a/b");
      assert ((List.nth es 3).file_path = "testdir1/c");

      let es = Cmd.list_entries "testdir1" in
      assert ((List.nth es 0).file_path = "testdir1/");
      assert ((List.nth es 1).file_path = "testdir1/a/");
      assert ((List.nth es 2).file_path = "testdir1/a/b");
      assert ((List.nth es 3).file_path = "testdir1/c");

      ());

  ()

let test_populate_zip_info _text_ctxt =
  with_wd "../../../test/testdata/testdir1" (fun () ->
      let es = Cmd.list_entries "." in
      let total_size, size_of_central_directory, needs_zip64_eocd =
        Cmd.populate_zip_info es
      in
      assert (total_size = 478);
      assert (size_of_central_directory = 216);
      assert (not needs_zip64_eocd);
      assert ((List.nth es 0).zip_local_file_header_offset = 0);
      assert ((List.nth es 1).zip_local_file_header_offset = 60);
      assert ((List.nth es 2).zip_local_file_header_offset = 151);
      assert ((List.nth es 0).zip_central_directory_entry_offset = 240);
      assert ((List.nth es 1).zip_central_directory_entry_offset = 312);
      assert ((List.nth es 2).zip_central_directory_entry_offset = 385);
      ());
  with_wd "../../../test/testdata/testdir2" (fun () ->
      let es = Cmd.list_entries "." in
      let total_size, size_of_central_directory, needs_zip64_eocd =
        Cmd.populate_zip_info es
      in
      assert (total_size = 5368709730);
      assert (size_of_central_directory = 250);
      assert needs_zip64_eocd;
      ());
  ()

let test_write_zip _text_ctxt =
  let e2e ?(expected_rsync_stdout = ".d..t...... ./") wd expected_size =
    with_wd wd @@ fun () ->
    let output_path = Filename.temp_file "zipar-test-" ".zip" in
    let output_unzip_path = output_path ^ ".unzip" in
    let output_rsync_path = output_path ^ ".rsync" in
    Fun.protect ~finally:(fun () ->
        command "rm"
          [ "-rf"; output_path; output_rsync_path; output_unzip_path ]
        |> ignore)
    @@ fun () ->
    (* Zip everything in the current directory *)
    let input_paths = [ "." ] in
    Cmd.(
      with_domainslib_pool (fun pool -> write_zip pool output_path input_paths));

    (* Make sure the zip file size is correct *)
    assert (read_regular_file_size output_path = expected_size);

    (* Unzip the archive we made *)
    let unzip_res = command "unzip" [ output_path; "-d"; output_unzip_path ] in
    assert (unzip_res = 0);

    (* Make sure the unzipped files are the same as the original ones *)
    let rsync_res =
      command ~stdout:output_rsync_path "rsync"
        [ "-acin"; "--delete"; wd ^ "/"; output_unzip_path ^ "/" ]
    in
    assert (rsync_res = 0);
    let ic = open_in_bin output_rsync_path in
    Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
    let rsync_stdout = In_channel.input_all ic in
    assert (String.trim rsync_stdout = String.trim expected_rsync_stdout);
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
  let open OUnit2 in
  run_test_tt_main
    ("zipar"
    >::: [
           "list_entries" >:: test_list_entries;
           "populate_zip_info" >:: test_populate_zip_info;
           "write_zip" >:: test_write_zip;
         ])
