#use "metoro_network.ml"
#use "ekikan.ml"

(*　駅の木を作成　*)
type eki_tree_t = Empty | Node of eki_tree_t * (string * (string * float) list) * eki_tree_t

(* 駅名と距離の組のリストから、*)
let rec assoc ekimei association_lst = match association_lst with
| [] -> infinity
| (e,k)::rest -> if e = ekimei then k else assoc ekimei rest

 
(* ekikan_listからeki_tree_tを作成できるように *)
let rec insert_ekikan_kiten eki_tree kiten shuten kyori = match eki_tree with
| Empty -> Node (Empty,(kiten,(shuten,kyori)::[]),Empty)
| Node (t1,(eki,near_lst),t2) -> 
  if kiten < eki then Node (insert_ekikan_kiten t1 kiten shuten kyori,(eki,near_lst),t2)
  else if kiten > eki then Node (t1,(eki,near_lst),insert_ekikan_kiten t2 kiten shuten kyori)
  else Node (t1,(eki,(shuten,kyori)::near_lst),t2)



(* val insert_ekikan :ekikan -> eki_tree_t -> eki_tree_t*)
let insert_ekikan eki_tree ekikan = match ekikan with
  {kiten = kiten;shuten = shuten;kyori = kyori} -> 
  insert_ekikan_kiten (insert_ekikan_kiten eki_tree shuten kiten kyori) kiten shuten kyori

(* 駅間の例 *) 
let ekikan1 = 
  {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3} 
let ekikan2 = 
  {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2} 
let ekikan3 = 
  {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2} 
 
(* テスト *) 
let tree1 = insert_ekikan Empty ekikan1 
let test1 = tree1 = 
Node (Empty, ("新大塚", [("池袋", 1.8)]), 
  Node (Empty, ("池袋", [("新大塚", 1.8)]), Empty)) 
let tree2 = insert_ekikan tree1 ekikan2 
let test2 = tree2 = 
  Node (Empty, ("新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)]), 
	Node (Empty, ("池袋", [("新大塚", 1.8)]), 
	      Node (Empty, ("茗荷谷", [("新大塚", 1.2)]), Empty))) 
let tree3 = insert_ekikan tree2 ekikan3 
let test3 = tree3 = 
  Node (Node (Empty, ("後楽園", [("茗荷谷", 1.8)]), Empty), 
	("新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)]), 
        Node (Empty, 
	      ("池袋", [("新大塚", 1.8)]), 
	      Node (Empty, 
		    ("茗荷谷", [("後楽園", 1.8); ("新大塚", 1.2)]), 
        Empty))) 


(* ekikant_tree_tと ekikan listをうけとって、
  受け取ったすべてのekikanを挿入したekikant_tree+_tを帰す
*)
let inserts_ekikan tree ekikan_lst = List.fold_right (fun ekikan tree -> insert_ekikan tree ekikan) ekikan_lst Empty

let globale_ekikan_tree = inserts_ekikan Empty global_ekikan_list

(* 漢字の駅名とekikan_tree_tをうけとって、その荷役間の距離を返す *)
(* val get_ekikan_kyori : ekikan_tree_t -> string -> string -> ->float*)
let rec get_ekikan_kyori tree kiten shuten = match tree with
| Empty -> infinity
| Node (t1,(ekimei,near_eki_lst),t2) -> 
  if kiten < ekimei 
    then get_ekikan_kyori t1 kiten shuten
  else if kiten > ekimei
    then get_ekikan_kyori t2 kiten shuten
  else assoc shuten near_eki_lst

let test_get_ekikan_kyori_1 = get_ekikan_kyori globale_ekikan_tree "池袋" "所沢" = infinity
let test_get_ekikan_kyori_1 = get_ekikan_kyori globale_ekikan_tree "所沢" "池袋" = infinity
let test_get_ekikan_kyori_1 = get_ekikan_kyori globale_ekikan_tree "池袋" "新大塚" = 1.8