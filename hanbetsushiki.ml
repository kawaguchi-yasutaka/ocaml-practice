(* 目的: 二次方程式の実数解の個数を求める*)
(* float -> float -> float -> int) *)
let hanbetsushiki a b c =
	if b ** 2. -. (4. *. a *. c) > 0. then 2
	else if b ** 2. -. (4. *. a *. c) < 0. then 0
	else 1



let test1 = hanbetsushiki 1.0 2.0 (-8.0) = 2
let test1 = hanbetsushiki 1. (-4.) 4. = 1
let test1 = hanbetsushiki 5.(-3.) 4. = 0