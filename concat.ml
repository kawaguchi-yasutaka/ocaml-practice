(* stirng list 
	 - []
	 - first::rest
*)
(* 文字列のリストを受け取って、先頭から順に結合して文字列を返す*)
let rec concat list = match list with
| [] -> ""
| first::rest -> first ^ concat rest


let concat_test1 = concat [] = ""
let concat_test2 = concat ["春";"夏";"秋";"冬"] = "春夏秋冬"