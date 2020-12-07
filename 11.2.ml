(*
  - n = 0 は3
  - 2an -1 -1
*)

let rec a n = 
  if n = 0 then 3
  else 2 * a (n - 1) - 1

let test_a1 = a 0 = 3
let test_a2 = a 1 = 5
let test_a3 = a 2 = 9
let test_a4 = a 3 = 17