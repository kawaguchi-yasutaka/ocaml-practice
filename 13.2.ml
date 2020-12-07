(* 人を表すレコード*)
type person_t = {
  namae : string;
	blood_type : string;
}
let get_namae person = person.namae

let rec map f list = match list with
| [] -> []
| first::rest -> f first::map f rest
let person_namae psersons = map sqrt psersons

let test1 = person_namae [{namae ="aa";blood_type="aa"}] = ["aa"]