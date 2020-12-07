
(*昇順のintリストと、intを受け取ってintリストに挿入してリストを返す*)
(* insert : int list -> int -> int list *)
let rec insert lst n = match lst with 
   [] -> [n]
   |first::rest -> if first >= n then n::first::rest else first::insert rest n

let test_issert1 = insert [] 1 = [1]
let test_issert2 = insert [1;3;4;7;8;] 5 = [1;3;4;5;7;8;]
let test_issert3 = insert [1;3;4;7;8;] 10 = [1;3;4;7;8;10;]
let test_issert4 = insert [1;3;4;7;8;] 0 = [0;1;3;4;7;8;]
let test_issert5 = insert [1;3;4;7;8;] 3 = [1;3;3;4;7;8;]



(* intリストを受け取ったら、昇順に整列したintリストを返す*)
(* ins_sort : int list -> int list*)
let rec ins_sort lst = match lst with
   [] -> []
   |first::rest -> insert(ins_sort rest) first
   ¥

let test_ins_sort1 = ins_sort [] = []
let test_ins_sort2 = ins_sort [5;3;8;1;7;4;] = [1;3;4;5;7;8;]
let test_ins_sort3 = ins_sort [1;3;4;5;7;8;] = [1;3;4;5;7;8;]


(* 学生を表すレコード*)
type gakusei_t = {
   name : string;
   tensu:int;
}

let rec gakusei_insert lst ({tensu = new_t} as new_gakusei) = match lst with
| [] -> [new_gakusei]
| ({tensu = t} as first)::rest -> if t >= new_t then new_gakusei::first::rest else first::gakusei_insert rest new_gakusei

let test_gakusei_insert1 = gakusei_insert [] {name = "満点男";tensu = 100;} = [{name = "満点男";tensu = 100}]
let test_gakusei_insert2 = gakusei_insert [{name = "満点男";tensu = 100;}] {tensu = 99;name = "満点男 -1"} =
    [{tensu = 99;name = "満点男 -1"};{name = "満点男";tensu = 100;}]
let test_gakusei_insert3 = gakusei_insert 
    [{tensu = 99;name = "満点男 -1"};{name = "満点男";tensu = 100;}] {tensu = 99;name = "満点男 -1"} =
      [{tensu = 99;name = "満点男 -1"};{tensu = 99;name = "満点男 -1"};{name = "満点男";tensu = 100;}]



(* 学生tのリストを受け取ってtensuフィールドを昇順に整列した学生tのリストを返す *)
(* gakusei_sort : gakusei_t list -> gakusei_t list*)
let rec gakusei_sort gakuseis = match gakuseis with
| [] -> []
| ({name = n;tensu = t} as first)::rest -> gakusei_insert (gakusei_sort rest) first 

let test_gakusei_sort1 = gakusei_sort [] = []
let test_gakusei_sort2 = gakusei_sort [{tensu = 100;name = "満点男"}] = [{tensu = 100;name = "満点男"}]
let test_gakusei_sort3 = gakusei_sort [{tensu = 100;name = "満点男"};{tensu = 99;name = "満点男 -1"}] = 
   [{tensu = 99;name = "満点男 -1"};{tensu = 100;name = "満点男"}]



