(* 引数にf関数、初期値init リストlst *)
(* val fold_left : (a -> b -> a) -> a -> b list*)
let rec fold_left f lst init = match lst with
| [] -> init
| first::rest -> f (fold_left f rest init) first

let test_fold_left1 = fold_left (^) [] "" = ""
let test_fold_left1 = fold_left (^) ["あ";"い";"う";"え";"お"] ""
