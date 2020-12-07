let rec fold_right f lst init = match lst with
| [] -> init
| first::rest -> f first (fold_right f rest init)  

let concat lst = fold_right (^) lst ""

let test_concat1 = concat ["あ";"い";"う";"え";"お";]