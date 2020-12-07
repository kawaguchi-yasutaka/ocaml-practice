let problem1 a = a
let problem2 a b = a
let problem2 a = let f b = a in f
let problem3 a f = f a


let problem4 f1 f2 a = f2 (f1 a)


