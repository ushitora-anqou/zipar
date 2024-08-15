(*open Zipar*)

let () =
  let open OUnit2 in
  run_test_tt_main ("zipar" >::: [])
