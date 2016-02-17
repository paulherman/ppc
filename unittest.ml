open OUnit2;;

let test1 test_ctxt = assert_equal "x" "x";;
let test2 test_ctxt = assert_equal "x" "y";;


(* Name the test cases and group them together *)
let suite =
    "suite" >::: [
        "test1" >:: test1;
        "test2" >:: test2
    ];;

let () = run_test_tt_main suite;;
