

let rec sumlist l =
    match l with
    | [] -> 0.0
    | x :: xs -> x + sumlist xs

let rec pairlists twolists =
    match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists (xs,ys) //why is this not working

let w_mean weights data =
    let midlist = pairlists (weights,data)
    let rec helper(l1,l2) =
        match (l1,l2) with
        | ([],[]) -> 0.0
        | (x::xs,y::ys) -> x*y + helper(xs,ys)
    let prod = helper(weights,data)
    prod/sumlist(weights)


let rec memberof pair =
    match pair with
    | (x,[]) -> false
    | (x,y::ys) -> if x=y then true else memberof(x,ys)


let rec remove(item, lst) =
    match lst with
    | x::xs -> if x=item then remove(item,xs) else x::remove(item,xs)
    | [] -> []

let findMax l =
    let rec helper(l,m) =
        match l with
        | [] -> m
        | x::xs -> match x with
                        | x when x > m -> helper(xs,x)
                        | _ -> helper(xs,m)
    match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)

let rec selsort l =
    match l with
    | [] -> []
    | l -> let max = findMax l
           max::selsort(remove(max,l))

let rec common twolists =
    match twolists with
    | ([],[]) -> []
    | (x,[]) -> []
    | ([],y) -> []
    | (x::xs,y) -> if memberof(x,y) then x::common(xs,y) else common(xs,y)

let rec split l =
    match l with
    | [] -> ([],[])
    | [x] -> ([x],[])
    | x::y::xs -> let l1,l2 = split xs
                  x::l1, y::l2

let rec merge twolists =
    match twolists with
    | (x,[]) -> x
    | ([],y) -> y
    | (x::xs,y::ys) -> if x <= y then x::merge(xs,y::ys)
                       else y::merge(x::xs,ys)

let rec mergesort l =
    match l with
    | [] -> []
    | (x::[]) -> x::[] (* Without this you will go into an infinite loop. *)
    | x::xs -> let (l1,l2) = split l
               in merge (mergesort l1,mergesort l2)
