#use "length.ml"
(* 二つのリストを受け取り、長さが同じか確認する*)
(* equal_length : 'a list -> 'a list -> bool *)
let equal_length lst1 lst2 = let lst1_lenght = length lst1 in let lst2_lenght = length lst2 in if lst1_lenght = lst2_lenght then true else false


let test_equal_length = equal_length [] [] = true
let test_equal_length1 = equal_length [1] [] = false
let test_equal_length2 = equal_length [] [1] = false
let test_equal_length3 = equal_length [1;1;1] [1;1;1] = true