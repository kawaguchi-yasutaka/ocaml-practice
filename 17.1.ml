type nengou_t = Showa of int | Heisei of int | Reiwa of int 

let to_seireki nengou = match nengou with
| Showa (n) -> n + 1925
| Heisei (n) -> n + 1988
| Reiwa (n) -> n + 2018


(* 年齢のnegou_ｔと現在のnego_tをうけとって年齢を返す *)
(* varl nenrai : negoo_t -> negou_t -> int *)
let nengou birth_nengou current_nengou = to_seireki current_nengou - to_seireki birth_nengou

let test_nengou_1 = nengou (Heisei 7) (Reiwa 2) = 25
