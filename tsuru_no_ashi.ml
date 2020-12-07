(* 目的: 鶴の数xの足の本数の合計を数える*)
(* tsuru_no_ashi : int -> int *)
let tsuru_no_ashi x = x * 2

let test1 = tsuru_no_ashi 0 = 0
let test2 = tsuru_no_ashi 1 = 2
let test3 = tsuru_no_ashi 20 = 40