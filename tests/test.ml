open OUnit;;

let suite = "ocaml-redis" >::: 
  [
    Smoke_test.suite;
  ]

let _ = run_test_tt_main suite

