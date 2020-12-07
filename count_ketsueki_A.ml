type birthday_t = {
	month : int;
	date : int;
}


(* 人を表すレコード*)
type person_t = {
	name : string;
	height : float;
	weight : float;
	birthday : birthday_t;
	bload_type : string;
}

(* 人person_tのリストを受け取って、結劇型がA型の人の数を返す*)
(* count_ketsueki_A : person_t list -> int *)
let rec count_ketsueki_A persons = match persons with
| [] -> 0
| {name = n;height = h;weight = w;birthday = b;bload_type = bt}::rest -> if bt = "A" then 1 + count_ketsueki_A rest else count_ketsueki_A rest


let count_ketsueki_A_test1 = count_ketsueki_A [] = 0
let count_ketsueki_A_test2 = count_ketsueki_A [
{name = "川口";height = 1.72; weight = 65.5;birthday = {month = 1;date = 16};bload_type = "A"};
{name = "川口";height = 1.72; weight = 65.5;birthday = {month = 1;date = 16};bload_type = "B"};
] = 1


(* 人person_tのリストを受け取ったらd、乙女座の人のリストを返す*)
(* otomeza : person_t list -> peson_t list*)
let rec otomeza persons = match persons with
| [] -> []
| ({name = n;height = h;weight = w;birthday = {month = m;date = d};bload_type = bt} as person)::rest
	-> if  m = 8 && (d >= 23 && d <= 31) || m = 9 && (d >= 1 && d <= 22) then person::otomeza rest
	   else otomeza rest



let otomeza_test1 = otomeza [] = []
let otomeza_test2 = otomeza [
{name = "川口";height = 1.72; weight = 65.5;birthday = {month = 8;date = 23};bload_type = "A"};
{name = "川口";height = 1.72; weight = 65.5;birthday = {month = 9;date = 23};bload_type = "A"};
] = [{name = "川口";height = 1.72; weight = 65.5;birthday = {month = 8;date = 23};bload_type = "A"};]

