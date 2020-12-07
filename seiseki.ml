(* 名前と成績の組みを受け取って、文字列を返す*)
(* seiseiki : string * string -> string *)
let seiseki pair = match pair with
(name,grade) -> name ^ "さんの評価は" ^ grade ^ "です"

let test1 = seiseki ("川口","△") = "川口さんの評価は△です"
