open OUnit2;;

(* Name the test cases and group them together *)
let suite =
    "Util.starts_with" >::: [
        "string shorter than pattern" >:: (fun ctx -> assert_equal (Util.starts_with "" "abc") false);
        "string not matching pattern" >:: (fun ctx -> assert_equal (Util.starts_with "abc" "abd") false);
        "string matching pattern same length" >:: (fun ctx -> assert_equal (Util.starts_with "abc" "ab") true);
        "string matching shorter pattern" >:: (fun ctx -> assert_equal (Util.starts_with "abc" "abc") true)
    ];;

let () = run_test_tt_main suite;;
