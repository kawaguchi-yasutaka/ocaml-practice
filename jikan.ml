(* 目的：時間hourを受け取ったら、午前or午後を返す*)
(* jikan : int -> string *)
let jikan hour = if hour < 11 then "午前" else "午後"


let test1 = jikan 12 = "午後"
let test2 = jikan 0 = "午前"
let test2 = jikan 23 = "午後"