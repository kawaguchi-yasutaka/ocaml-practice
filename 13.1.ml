(* 人を表すレコード*)
type person_t = {
	blood_type : string;
}

(* person_tのリストを受け取ったら、指定された血液型の人の数を返す*)
(* count_ketsueki : person_t list -> string -> int*)
let rec count_ketsueki persons blood_type = match persons with
| [] -> 0
| ({blood_type = b})::rest -> 
  if b = blood_type then 1 + count_ketsueki rest blood_type
  else count_ketsueki rest blood_type

let test_count_ketsueki1 = count_ketsueki [] "A" = 0
let test_count_ketsueki1 = count_ketsueki [{blood_type="A"};{blood_type="A"}] "A" = 2
let test_count_ketsueki1 = count_ketsueki [{blood_type="A"};{blood_type="A"}] "b" = 0
