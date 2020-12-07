(* 入力データー型
	int list
	: []
	: first::rest (restが自己参照している == 関数で受け取った同じ型である == こいつに再帰呼び出しが可能)
*)

(* 目的： 整数のリストを受け取って、 そのリストの長さを返す*)
(* length : 'a list -> int *)
let rec length list = match list with
| [] -> 0
| first::rest -> 1 + length rest


let length_test1 = length [] = 0
let length_test2 = length [1;] = 1
let length_test2 = length [1;2;2;3;4;5;5;5;5;] = 9