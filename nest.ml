(* 接頭辞のりすとを受け取って,nを先頭につける*)
(* add_to_each : int -> int list list -> int list list*)
let rec add_to_each n lst = match lst with
    [] -> []
    |first::rest -> (n::first)::add_to_each n rest (* add_to_each n rest *)

let test_add_to_each1 = add_to_each 0 [] = []
let test_add_to_each2 = add_to_each 0 [[1]] = [[0;1]]
let test_add_to_each2 = add_to_each 0 [[1];[1;2]] = [[0;1];[0;1;2]]


(* intリストを受け取って、接頭語のリストをかえす*)
(* prefix : int list -> int list list*)
let rec prefix lst = match lst with 
    [] -> [[]]
    |first::rest -> [first]:: add_to_each first (prefix  )

let test_prefix1 = prefix [] = []
let test_prefix1 = prefix [1] = [[1]]
let test_prefix1 = prefix [1;2] = [[1];[1;2]]
let test_prefix1 = prefix [1;2;3] = [[1];[1;2];[1;2;3]]
