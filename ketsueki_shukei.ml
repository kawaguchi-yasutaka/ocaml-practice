type person_t = {
  name: string;
  blood_type: string;
}

(* person_tのリストを受け取って、各血液型の人数を集計して返す*)
(* (A,B,O,AB)*)
(* ketsueki_shukei : person_t list -> (int * int * int * int)*)
let rec ketsueki_shukei lst = match lst with
| [] -> (0,0,0,0)
| ({name = n;blood_type = bt})::rest -> 
  let (a,b,o,ab) = ketsueki_shukei rest in 
  if bt = "A" then (a +1 ,b,o,ab) 
  else if bt = "B" then (a ,b + 1,o,ab) 
  else if bt = "O" then (a ,b,o + 1,ab) 
  else (a ,b,oab + 1)  

let test_ketsueki_shukei1 = ketsueki_shukei [] = (0,0,0,0)
let test_ketsueki_shukei2 = ketsueki_shukei [
  {name = "川口";blood_type = "A"};
  {name = "川口";blood_type = "B"};
  {name = "川口";blood_type = "O"};
  {name = "川口";blood_type = "AB";
  }] = (1,1,1,1)
