(* 自然数mとnを受けとって、ユークリッドの互除法で最大公約数を求める *)
(* let gcd : int -> int -> int *)
let rec gcd m n = 
  let next_n = m mod n in
  if next_n = 0 
    then n 
  else 
    gcd n next_n

let test_gcd1 = gcd 2 1 = 1
let test_gcd2 = gcd 2 1 = 1
let test_gcd3 = gcd 3 2 = 1
let test_gcd4 = gcd 465 360 = 15

(* じめに答えが出るケースmとnの余りが0 *)
(* その時の答えは、n *)
(* 部分問題は、n mod (m mod n) = n *)
