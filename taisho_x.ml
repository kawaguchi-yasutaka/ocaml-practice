(* x軸とy軸の組の座標を受け取り、x軸に対象な座礁を求める*)
(* (int * int) -> (int * int) *)
let tasiho_x point = match point with
 (x, y) -> (x, -(y))

let test1 = tasiho_x (1,1) = (1, -1)
let test2 = tasiho_x (1,0) = (1,0)
let test3 = tasiho_x (1, -1) =(1, 1)
