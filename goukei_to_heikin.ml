(* 5科目の点数a b c d fを受け取って合計点と平均点を計算する*)
(* goukei_to_heikin : int -> int -> int -> int -> int -> int * int *)
let goukei_to_heikin a b c d e = (a + b + c + d + e, (a + b + c + d + e) / 5)


let tes1 = goukei_to_heikin 100 90 80 70 60 = (400,80)