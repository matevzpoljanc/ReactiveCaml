open ReactiveCaml
open OUnit2
open Core_kernel

let x0 = return 7
let x1 = return 8
let x2 = return 9
let x3 = return 1
let x4 = return 2
let sum3 = map3 x0 x1 x2 (fun x y z -> x + y + z)
let sum2 = map2 x3 x4 (fun x y -> x + y)
let result = map2 sum2 sum3 ~f:(fun x y -> x*y)
let print_int_t t = print_endline @@ string_of_int @@ read_exn @@ t

let test_init () = set_value x0 7; set_value x1 8; set_value x2 9; set_value x3 1; set_value x4 2

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

let benchmark_function ~f ~args =
    let timeit f =
    (let start_t = Sys.time () in
    ignore @@ f (); Sys.time () -. start_t) in
    let number_of_iterations = 10000 in
    let test_scores =  List.map args ~f:(fun n -> List.init number_of_iterations ~f:(fun x -> timeit @@ fun () -> f n)) in (* Replicate the test multiple times *)
    let average_run_time = 
        List.map ~f:(fun x -> x /. float_of_int number_of_iterations) @@ (* Average results *)
            List.map ~f:(fun el -> List.fold ~init:0.0 ~f:(+.) el) test_scores in (* Add all test results for one parameter *)
    let std_dev = 
        List.map2_exn test_scores average_run_time ~f:(fun scores mean -> List.fold scores ~init:0.0 ~f:(fun acc el -> acc +. (el -. mean) ** 2.)) in
    let max_run_time = List.map ~f:(fun x -> match x with | Some y -> y | None -> -1.0) @@ List.map test_scores ~f:(List.max_elt ~cmp:(fun x y -> if x>y then 1 else if x=y then 0 else -1)) in
    let min_run_time =  List.map ~f:(fun x -> match x with | Some y -> y | None -> -1.0) @@ List.map test_scores ~f:(List.min_elt ~cmp:(fun x y -> if x>y then 1 else if x=y then 0 else -1)) in
    ignore @@ List.map3 
        ~f:(fun mean dev (max,min) -> print_endline @@ "mean: " ^ string_of_float mean ^ " stdev: " ^ string_of_float dev ^ " max: " ^ string_of_float max ^ " min: " ^ string_of_float min) 
        average_run_time std_dev (List.map2_exn max_run_time min_run_time ~f:(fun x y -> (x,y)))


let () =
    benchmark_function ~f:(fun n -> set_value x0 (Random.int 1000); read_exn result) ~args:[1];
    test_init ();
    run_test_tt_main suite
