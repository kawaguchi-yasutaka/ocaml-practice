(* 入力データ型
	int list
		- [] (自己参照していないケース)
		- first::rest (自己参照しているケース)
*)

(* 目的: 整数リストから、偶数の要素を見つけて偶数のリストを返す*)
(* even : int list -> int list*)
let rec even list = match list with
| [] -> []
| first::rest -> if first mod 2 = 0 then first::even rest else even rest

let even_test1 = even [] = []
let even_test2 = even [0;1;2;4] = [0;2;4]
let even_test2 = even [1;3;5] = []


