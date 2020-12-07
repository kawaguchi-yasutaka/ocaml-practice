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


(* 目的: personを受け取って血液型をを表す文字列返す*)
(* ketsueki_houji : person_t -> string*)
let ketsueki_houji person = match person with
 {name = n; height = h; weight = w;birthday = b; bload_type = bt} -> n ^ "さんの血液型は"^bt^"型です"

let tes1 = ketsueki_houji {name = "川口";height = 1.72;weight = 65.5;birthday = {month = 1;date = 16};bload_type = "A"} = "川口さんの血液型はA型です"