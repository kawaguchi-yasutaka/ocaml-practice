let rec power m n = 
  if n = 0 then 1
  else m * power m (n - 1)  

let rec sum_of_squre n = 
  if n = 0 then 0
  else (power n 2) + sum_of_squre (n - 1)
let test_sum_of_squre1 = sum_of_squre 0 = 0
let test_sum_of_squre2 = sum_of_squre 1 = 1
let test_sum_of_squre3 = sum_of_squre 4 = 30

3
5
9
17

