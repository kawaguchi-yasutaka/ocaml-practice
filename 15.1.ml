(* 受け取っとリストを、クイックソートを使って昇順に並び替える*)
(* val quick_sort : 'a list -> 'a list *)
let rec quick_sort lst = match lst with
| [] -> []
| first::rest -> 
  let take_less lst = List.filter (fun l -> l <= first) lst in (* quik_sort take_less rest *)
  let take_grater lst = List.filter (fun l -> l > first) lst in (* quick_sort taktake_grater rest *)
  quick_sort (take_less rest)@[first]@quick_sort (take_grater rest)


let test_quick_sort1 = quick_sort [] = []
let test_quick_sort2 = quick_sort [1] = [1]
let test_quick_sort3 = quick_sort [1;2] = [1;2]
let test_quick_sort4 = quick_sort [2;1] = [1;2]
let test_quick_sort5 = quick_sort [5;4;9;8;2;3] = [2;3;4;5;8;9]
let test_quick_sort5 = quick_sort [4;4;] = [4;4]
