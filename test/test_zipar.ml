open Zipar

let with_wd path f =
  let old_wd = Unix.getcwd () in
  Unix.chdir path;
  Fun.protect ~finally:(fun () -> Unix.chdir old_wd) @@ fun () -> f ()

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

let test_populate_zip_offsets _text_ctxt =
  with_wd "../../../test/testdata/testdir1" (fun () ->
      let es = Cmd.list_entries "." in
      let total_size = Cmd.populate_zip_offsets es in
      assert (total_size = 478);
      assert ((List.nth es 0).zip_local_file_header_offset = 0);
      assert ((List.nth es 1).zip_local_file_header_offset = 60);
      assert ((List.nth es 2).zip_local_file_header_offset = 151);
      assert ((List.nth es 0).zip_central_directory_entry_offset = 240);
      assert ((List.nth es 1).zip_central_directory_entry_offset = 312);
      assert ((List.nth es 2).zip_central_directory_entry_offset = 385);
      ());
  ()

let test_write_zip _text_ctxt =
  with_wd "../../../test/testdata/testdir1" (fun () ->
      let output_path = Filename.temp_file "zipar-test-" ".zip" in
      Fun.protect ~finally:(fun () -> Unix.unlink output_path) @@ fun () ->
      let input_paths = [ "." ] in
      Cmd.write_zip output_path input_paths;
      let expected_output_path = "../testdir1-inside.zip" in
      let diff_res =
        Sys.command
          (Filename.quote_command "cmp" [ output_path; expected_output_path ])
      in
      assert (diff_res = 0);
      ());
  ()

let () =
  let open OUnit2 in
  run_test_tt_main
    ("zipar"
    >::: [
           "list_entries" >:: test_list_entries;
           "populate_zip_offsets" >:: test_populate_zip_offsets;
           "write_zip" >:: test_write_zip;
         ])
