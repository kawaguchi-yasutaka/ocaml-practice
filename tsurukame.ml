(* 目的： 鶴と亀の合計xと足の数の合計yから、鶴の数を計算する*)
(* tsurukame : int -> int -> int *)
let tsurukame x y = (x * 4 - y) / 2


let test2 = tsurukame 2 8 = 0
let test1 = tsurukame 2 6 = 1
let test2 = tsurukame 2 4 = 2

