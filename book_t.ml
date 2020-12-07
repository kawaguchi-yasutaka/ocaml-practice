type book_t = {
	title : string;
	author : string;
	publisher : string;
	price : int;
	isbn : int;
}

let kwaguchi = {title = "川口";author = "川口"; publisher = "出版社"; price = 1000; isbn = 111111111111}


type okozukai_t = {
	name : string;
	price : int;
	place : string;
}

let riongo = {name = "りんご";price = 100;place = "ドンキーほーて"}

type birthday_t = {
	month : int;
	date : int;
}

type person_t = {
	name : string;
	height : float;
	weight : float;
	birthday : birthday_t;
	bload_type : string;
}

let ore = {name = "川口";height = 1.72;weight = 65.4;birthday = {month = 1;date = 16};bload_type = "A"}