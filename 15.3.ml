(* n以下の素数全てをエラトステネスのふるいで求める*)
(* let prime : int -> int list*)
(* let init: int -> int list *)
(* let remove_not_prime : int list  -> int list*)


let rec insert lst n = match lst with 
   [] -> [n]
   |first::rest -> if first >= n then n::first::rest else first::insert rest n

   let rec ins_sort lst = match lst with
   [] -> []
   |first::rest -> insert(ins_sort rest) first
let prime n = 
  let rec init n = if n <= 1 then [] else n::init(n- 1) in 
  let rec add_first_to_result lst result = match lst with
  | [] -> result
  | first::rest -> let rec remove_not_prime a lst = (match lst with
  | [] -> []
  | first::rest -> if first mod a = 0 then remove_not_prime a rest else first::remove_not_prime a rest
  ) in
  first::add_first_to_result (remove_not_prime first rest) result in
  add_first_to_result (ins_sort (init n)) []
  
let test_prime1 = prime 1 = []
let test_prime1 = prime 2 = [2]
let test_prime1 = prime 3 = [2;3]
let test_prime1 = prime 10 = [2;3;5;7]
