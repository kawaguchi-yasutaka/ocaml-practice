(* 学生を表すレコード*)
type gakusei_t = {
   name : string;
   tensu:int;
}

(* 学生tのリストを受け取って、最高特定のをとった学生レコードを返す*)
(* gakusei_max : gakusei_t list -> gakusei_t *)
let rec gakusei_max lst = match lst with
| [] -> {name = "デフォルト";tensu = 0}
| ({name = n;tensu = t} as first)::rest -> 
   let max_g = gakusei_max rest in 
   if t >= max_g.tensu then first 
   else max_g

let test_gakusei_max1 = gakusei_max [] = {name = "デフォルト";tensu = 0}
let test_gakusei_max1 = gakusei_max [{name = "川口";tensu = 99};{name = "坂本";tensu = 100}] = {name = "坂本";tensu = 100}
