let rec filter f lst = match lst with
|[] -> []
|first::rest -> if f first then first::filter f rest else filter f rest

let even lst = filter (fun a -> a mod 2 = 0) lst