(* 目的: 亀の数xの足の本数の合計を数える*)
(* tsuru_no_ashi : int -> int *)
let kame_no_ashi x = x * 4


let test1 = kame_no_ashi 0 = 0
let test2 = kame_no_ashi 1 = 4
let test3 = kame_no_ashi 20 = 80