type 'a tree_t = Empty 
| Leaf of 'a
| Node of 'a tree_t * 'a * 'a tree_t

(* 'a tree_tは
      - empty  空の木
      - Leaf ('a)　値がnの葉
      - Node (t1,n,t2)  左がt1 値n 右がt2の節(t1とt2が自己参照)
*)

let tree1 = Empty
let tree2 = Leaf (3)
let tree3 = Node (tree1,4,tree2)
let tree4 = Node(tree2,5,tree3)

(* tree_tを受け取ったら、値をすべて2倍にした木を返す *)
(* val tree_doble : tree_t -> tree_t*)
let rec tree_duble tree = match tree with
| Empty -> tree
| Leaf (n) -> Leaf (n * 2)
| Node (t1,n,t2) -> Node (tree_duble t1, n * 2, tree_duble t2)

let test_tree_doble1 = tree_duble  tree1 = Empty
let test_tree_doble2 = tree_duble tree2 = Leaf (6)
let test_tree_doble3 = tree_duble tree3 = Node (Empty,8,Leaf(6))

(* int -> intの関数fとtree_tを受け取って、すべての値にfを適用したtree_tを返す*)

let add3 n = n + 3
let rec tree_map f tree = match tree with
| Empty -> tree
| Leaf (n) -> Leaf (f (n))
| Node (t1,n,t2) -> Node (tree_map f t1, f (n),tree_map f t2)

let rec tree_length tree = match tree with
| Empty -> 0
| Leaf (n) -> 1
| Node (t1,n,t2) -> 1 + tree_length t1 + tree_length t2

let rec tree_depth tree = match tree with
| Empty -> 0
| Leaf (n) -> 1
| Node (t1,n,t2) -> let left_depth = tree_depth t1 in let right_depth = tree_length t2 in 
if left_depth < right_depth then  right_depth else left_depth

let test_tree_doble1 = tree_depth  tree1 = 0
let test_tree_doble2 = tree_depth tree2 = 1
let test_tree_doble3 = tree_depth tree3 = 1



(* 二分探索木 *)

(* 値と二分探索木を受け取って、値が存在するか検索する*)
(* val search : int -> tree_t -> bool *)
let rec search_tree n tree = match tree with
| Empty -> false
| Leaf (tn) -> if n = tn then true else false
| Node(t1,tn,t2) -> 
  if tn = n then true
  else if tn < n then search_tree n t2 else search_tree n t1

let search_tree_data1 = Empty
let search_tree_data2 = Leaf (2)
let search_tree_data3 = Node(Leaf(1),3,Leaf(4))
let search_tree_data4 = Node(search_tree_data3,5,Node(Leaf(7),8,Leaf(9)))
let test_search_tree1 = search_tree 1 search_tree_data1 = false
let test_search_tree2 = search_tree 2 search_tree_data2 = true
let test_search_tree3 = search_tree 3 search_tree_data3 = true
let test_search_tree4 = search_tree 1 search_tree_data3 = true
let test_search_tree5 = search_tree 4 search_tree_data3 = true
let test_search_tree6 = search_tree 9 search_tree_data4 = true

(* 値と二分探索木を受け取って、値が二分探索木に挿入して二分探索木を返す*)
(* val insert_tree : int -> tree_t -> tree_t *)
let rec insert_tree n tree = match tree with
| Empty -> Leaf (n)
| Leaf (tn) -> 
  if tn = n then Leaf(tn) 
  else 
    if tn < n then Node (Empty,tn,Leaf(n)) 
    else Node (Leaf(n),tn,Empty)
| Node (t1,tn,t2) -> 
  if tn = n then Node (t1,tn,t2)
  else
    if tn < n then Node (t1,tn,insert_tree n t2)
    else Node (insert_tree n t1,tn,t2)
let test_insert_tree1 = insert_tree 1 Empty = Leaf (1)
let test_insert_tree2 = insert_tree 3 (Leaf 3) = Leaf (3)
let test_insert_tree3 = insert_tree 1 (Leaf 3) = Node (Leaf(1),3,Empty)
let test_insert_tree4 = insert_tree 4 (Leaf 3)= Node (Empty,3,Leaf(4))
let test_insert_tree4 = insert_tree 4 (Node (Empty,3,Empty)) = Node(Empty,3,Leaf(4))
let test_insert_tree4 = insert_tree 2 (Node (Empty,3,Empty)) = Node(Leaf(2),3,Empty)

