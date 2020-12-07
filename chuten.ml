(* 座標を二つ受け取り、中間地点を求める *)
(* (float * float) -> (float * float)-> (float * float) *)
let chuten point1 point2 = match point1 with
 (x1,y1) -> match point2 with
		    (x2,y2) -> ((x1 +. x2) /. 2. , (y1 +. y2) /. 2.)

let test1 = chuten (1.,1.) (2.,2.) = (1.5,1.5)