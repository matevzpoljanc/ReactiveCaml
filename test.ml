open Reactivecaml
open OUnit2
let x = return 2

let y = map x (fun x -> x+1)


let z = map2 x y (fun x y -> x+y)

let x0 = return 7
let x1 = return 8
let x2 = return 9
let x3 = return 1
let x4 = return 2
let sum3 = map3 x0 x1 x2 (fun x y z -> x + y + z)
let sum2 = map2 x3 x4 (fun x y -> x + y)
let result = map2 sum2 sum3 ~f:(fun x y -> x*y)
let print_int_t t = print_endline @@ string_of_int @@ read_exn @@ t

let test_sum3 test_ctx = assert_equal 24 (read_exn sum3)
let test_sum2 test_ctx = assert_equal 3 (read_exn sum2)
let test_result tect_ctx = assert_equal (24*3) (read_exn result)
let test_changex0 test_ctx = set_value x0 4; assert_equal (21,63) ((read_exn sum3), read_exn result)
let test_changex3 test_ctx = set_value x3 5; assert_equal (7, 21*7) (read_exn sum2, read_exn result)

let suite = 
    "suite" >:::
    [
        "test_sum3" >:: test_sum3;
        "test_sum2" >:: test_sum2;
        "test_result" >:: test_result;
        "test_changex0" >:: test_changex0;
        "test_changex3" >:: test_changex3
    ]

let () =
    run_test_tt_main suite
