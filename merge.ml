
let rec merge lst1 lst2 = match (lst1,lst2) with
| ([],[]) -> []
| (first1::rest1,[]) -> lst1
| ([],first2::rest2) -> lst2
| (first1::rest1,first2::rest2) -> 
  if first1 < first2 then first1::merge rest1 lst2 else first2::merge lst1 rest2

let test_merge1 = merge [] [] = []
let test_merge2 = merge [1;2] [] = [1;2]
let test_merge2 = merge [] [3;4] = [3;4]
let test_merge2 = merge [2;3] [1;4] = [1;2;3;4]
let test_merge2 = merge [1;4] [2;3] = [1;2;3;4]
let test_merge2 = merge [1;3] [2;4] = [1;2;3;4]
let test_merge2 = merge [1;3] [2;4] = [1;2;3;4]
