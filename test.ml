open Reactivecaml2
let x = return 2

let y = map x (fun x -> x+1)


let z = map2 x y (fun x y -> x+y)

let x0 = return 7
let x1 = return 8
let x2 = return 9
let x3 = return 1
let x4 = return 2
let sum3 = map3 x0 x1 x2 (fun x y z -> print_endline @@ String.concat "_" (List.map (string_of_int) [x;y;z]); (x + y + z))
let sum2 = map2 x3 x4 (fun x y -> print_endline @@ String.concat "#" (List.map (string_of_int) [x;y]); (x + y))
let result = map2 sum2 sum3 ~f:(fun x y -> print_endline @@ String.concat "$" (List.map (string_of_int) [x;y]); x*y)
let print_int_t t = print_endline @@ string_of_int @@ read_exn @@ t

let () =
    print_int_t result;
    set_value x0 4; print_int_t result;
    set_value x3 5; print_int_t result
