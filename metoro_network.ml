#use "ekikan.ml"
#use "ekimei.ml"

(* 目的： 駅名 ekimei_tを受け取ったらユーザーがわかりやすい表示用の文字列を返す*)
(* hyouji : ekimei_t -> string *)
let hyouji ekimei = match ekimei with
 {kanji = kanji;kana = kana;romaji = romaji;shozoku = shuzoku;} -> shuzoku^","^kanji^"("^kana^")"

let houji_test1 = hyouji {kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線"} = "丸ノ内線,茗荷谷(みょうがだに)"



(* 目的 ローマ字の駅名と駅名リストを受け取って、駅名リストから漢字表記の駅名を返す*)
(* romaji_to_kanji : string -> ekimei_t list -> string*)
let rec romaji_to_kanji romaji ekimei_list = match ekimei_list with
| [] -> ""
| ({romaji = r;kanji = k})::rest -> if r = romaji then k else romaji_to_kanji romaji rest
let test_romaji_to_kanji1 = romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷"
let test_romaji_to_kanji2 = romaji_to_kanji "tekitou" global_ekimei_list = ""



(* 目的 感じで駅名二つと駅館リストを受け取り、2駅間の距離を返す*)
(* get_ekikan_kyori : sting -> string -> ekikan_t list -> int *) 
let rec get_ekikan_kyori kiten shuten ekikan_list = match ekikan_list with
| [] -> infinity
| ({kiten = t;shuten = s;kyori = k})::rest -> 
	if t = kiten && s = shuten then k 
	else if s = kiten && t = shuten then k
	else get_ekikan_kyori kiten shuten rest
let test_global_ekikan_list2 = get_ekikan_kyori "池袋" "御茶ノ水" global_ekikan_list = infinity
let test_global_ekikan_list2 = get_ekikan_kyori  "茗荷谷" "新大塚" global_ekikan_list = 1.2



(* 目的 *)
(* kyori_wo_houji string -> string -> string*)
let kyori_wo_houji kiten shuten = 
	let kiten_kanji = romaji_to_kanji kiten global_ekimei_list in 
	let shuten_kanji = romaji_to_kanji shuten global_ekimei_list in
		if kiten_kanji = "" then kiten^"という駅は存在しません"
		else if shuten_kanji = "" then shuten^"という駅は存在しません"
		else let kyori = get_ekikan_kyori kiten_kanji	shuten_kanji global_ekikan_list in 
			if kyori = infinity then kiten_kanji^"駅と"^shuten_kanji^"駅はつながっていません。" 
			else kiten_kanji^"駅から"^shuten_kanji^"駅まで"^string_of_float kyori^"kmです"

let test_kyori_wo_houji1 = kyori_wo_houji "ikebukuro" "ochanomizu" = "池袋駅と御茶ノ水駅はつながっていません。"
let test_kyori_wo_houji2 = kyori_wo_houji "myogadani" "shinotsuka" = "茗荷谷駅から新大塚駅まで1.2kmです"
let test_kyori_wo_houji3 = kyori_wo_houji "ikebukuro" "unknown" = "unknownという駅は存在しません"



(* グラフの最短経路を求めるのに必要なる駅を表すレコード*)
type eki_t = {
	namae : string; 
	saitan_kyori : float;
	temae_list : string list;
}

(* ekimei listから、eki_listを求める*)
(* make_eki_list : ekimei list -> eki list*)
(* let  make_eki_list ekimei_lst = match ekimei_lst with
| ({kanji = k})::rest -> {namae = k; saitan_kyori = infinity; temae_list = []}::make_eki_list rest *)

let  make_eki_list ekimei_lst = List.map (fun ekimei -> match ekimei with ({kanji = k}) -> {namae = k; saitan_kyori = infinity; temae_list = []}) ekimei_lst
 
let test_make_eki_list1 = make_eki_list [] = []
let test_make_eki_list2 = make_eki_list 
[{kanji="茅場町"; kana="かやばちょう"; romaji="kayabacho"; shozoku="東西線"}] = 
[{namae = "茅場町";saitan_kyori = infinity;temae_list = []}]

(* ekiリストと始点の駅名を受けっとて初期化する*)
(* shokika : eki_t list -> string -> eki_t list *)
let rec shokika eki_lst shiten = match eki_lst with
| [] -> []
| ({namae = n} as first)::rest -> 
	if n = shiten then {namae = n;saitan_kyori=0.;temae_list=[n]}::shokika rest shiten 
	else first::shokika rest shiten

let shokika eki_lst shiten = List.map (
	fun eki -> match eki with
	| {namae = n}-> if n = shiten then {namae = n;saitan_kyori=0.;temae_list=[n]} else eki
) eki_lst
let test_shokika1 = shokika [{namae = "茅場町";saitan_kyori = infinity;temae_list = []}] "茅場町" = 
	[{namae = "茅場町";saitan_kyori = 0.;temae_list = ["茅場町"]}]
let test_shokika2 = shokika [{namae = "茅場町";saitan_kyori = infinity;temae_list = []}] "not found" = 
	[{namae = "茅場町";saitan_kyori = infinity;temae_list = []}]

let make_initial_eki_list ekimei_list shiten = shokika (make_eki_list ekimei_list) shiten






let rec ekimei_insert lst ekimei = match lst with
| [] -> [ekimei]
| ({kana = k} as first)::rest -> 
	if k = ekimei.kana then ekimei_insert rest ekimei
	else if k < ekimei.kana then first::ekimei_insert rest ekimei
	else ekimei::lst

(* ekimeiリストを受け取って、駅名の重複を排除したもekimei listを返す*)
let rec seiretsu lst = match lst with
| [] -> []
| ({kana = k} as first)::rest -> ekimei_insert(seiretsu rest) first

let test_seiretsu1 = seiretsu [] = []
let test_seiretsu2 = seiretsu  [
	{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"}; 	
	{kanji="銀座"; kana="ぎんざ"; romaji="ginza"; shozoku="丸ノ内線"}; 
	{kanji="赤羽岩淵"; kana="あかばねいわぶち"; romaji="akabaneiwabuchi"; shozoku="南北線"};
	{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"}; 
] = [
	{kanji="赤羽岩淵"; kana="あかばねいわぶち"; romaji="akabaneiwabuchi"; shozoku="南北線"};
	{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"}; 	
	{kanji="銀座"; kana="ぎんざ"; romaji="ginza"; shozoku="丸ノ内線"}; 
]


(* 13.6 koushin1 *)

(* let koushin1 : eki_t -> eki_t -> eki_t *)
let kousin1 p q = match (p,q) with
	({namae = pn;saitan_kyori =ps;temae_list = pl},{namae = qn;saitan_kyori =qs;temae_list = ql}) -> 
		let pqkyori = get_ekikan_kyori pn qn global_ekikan_list in 
		if pqkyori = infinity then q
		else if ps +. pqkyori < qs then {namae = qn;saitan_kyori = ps +. pqkyori;temae_list=qn::pl}
		else q
let shiten_koushin = {namae = "池袋";saitan_kyori = 0.; temae_list = ["池袋"]}
(* 直前に確定した点と繋がっていない *)
let test_koushin1_1 = kousin1 shiten_koushin {namae = "埼玉";saitan_kyori = infinity;temae_list=[]} = {namae = "埼玉";saitan_kyori = infinity;temae_list=[]}

(* 直前に確定した点と繋がっていて、最短距離を更新する*)
let test_koushin1_2 = kousin1 shiten_koushin {namae = "新大塚";saitan_kyori = infinity;temae_list=[]} = 
	{namae = "新大塚";saitan_kyori = 1.8;temae_list=["新大塚";"池袋"]}
(* 直前に確定した点と繋がっているが、最短距離でないので更新しない*)
let test_koushin1_3 = kousin1 shiten_koushin {namae = "新大塚";saitan_kyori = 1.0;temae_list=["新大塚";"適当"]} = 
	{namae = "新大塚";saitan_kyori = 1.0;temae_list=["新大塚";"適当"]}


	(* 13.7 koushin *)
	(* 直前に最短距離が確定した、eki_tを未確定のeki_t listを受け取ってkousih1を適用してlist_tを返す *)
	let rec kousin prev_shortest_eki eki_lst = match eki_lst with
	| [] -> []
	| first::rest -> 	(fun p q -> match (p,q) with
	({namae = pn;saitan_kyori =ps;temae_list = pl},{namae = qn;saitan_kyori =qs;temae_list = ql}) -> 
		let pqkyori = get_ekikan_kyori pn qn global_ekikan_list in 
		if pqkyori = infinity then q
		else if ps +. pqkyori < qs then {namae = qn;saitan_kyori = ps +. pqkyori;temae_list=qn::pl}
		else q) prev_shortest_eki first::kousin prev_shortest_eki rest
	let test_kousin1 = kousin shiten_koushin [] = []
	let test_kousin2 = kousin shiten_koushin [{namae = "埼玉";saitan_kyori = infinity;temae_list=[]};{namae = "新大塚";saitan_kyori = infinity;temae_list=[]}] = 
		[{namae = "埼玉";saitan_kyori = infinity;temae_list=[]};{namae = "新大塚";saitan_kyori = 1.8;temae_list=["新大塚";"池袋"]}]


(* 15.4 eki_t リストを受けとって(最多距離最小の駅(eki_t),最多距離が未確認のりすと)の組みを返す*)
(* var saitan_wo_bunri : eki_t list -> ekit_t * ekit_t list*)
let saitan_wo_bunri lst = 
	let rec saitaneki lst = match lst with
		| [] -> {namae = "infinity";saitan_kyori = infinity;temae_list =[]}
		| ({saitan_kyori = s} as first)::rest -> let saitan = saitaneki rest in if s <= saitan.saitan_kyori then first else saitan in 
	let rec remove lst saitan = match lst with
		| [] -> []
		| first::rest -> if first = saitan then remove rest saitan else first::remove rest saitan in
	let saitan = saitaneki lst in (saitan,remove lst saitan)

let saitan_wo_bunri_fold_right lst = 	

let test_saitan_wo_bunri = saitan_wo_bunri [] = ({namae = "infinity";saitan_kyori = infinity;temae_list=[]},[])
let test_saitan_wo_bunri2 =  saitan_wo_bunri [{namae = "埼玉";saitan_kyori = infinity;temae_list=[]};{namae = "新大塚";saitan_kyori = 1.8;temae_list=["新大塚";"池袋"]}]
= ({namae = "新大塚";saitan_kyori = 1.8;temae_list=["新大塚";"池袋"]}, [{namae = "埼玉";saitan_kyori = infinity;temae_list=[]}])






