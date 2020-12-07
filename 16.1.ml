(* 整数リストを受け取って、それまでの数の合計からなるリストを返す*)
(* val sum_list : int list -> int list *)
let sum_list lst = let rec hojo lst sum = match lst with
| [] -> []
| first::rest -> sum + first::hojo rest (sum + first) in
hojo lst 0
let test_sim_list1 = sum_list [] = []
let test_sim_list2 = sum_list [3;2;1;4] = [3;5;6;10]