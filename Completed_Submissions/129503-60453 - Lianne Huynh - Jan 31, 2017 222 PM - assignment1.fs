(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Lianne Huynh, Id Number: 260683320 *)

(* Question 1 *) (* Do not edit this line. *)

let rec sumlist l =
  match l with
  | [] -> 0.0
  | x::xs -> x + sumlist xs

let rec pairlists twolists =                  
  match twolists with
  | ([], []) -> []
  | ([],x::xs) -> failwith "Error -- lists are not of the same length"
  | (x::xs, []) -> failwith "Error -- lists are not of the same length"
  | (x::xs, y::ys) -> (x,y)::pairlists(xs, ys)

let w_mean weights data =
  let lst = pairlists(weights, data)
  let lst2 = lst |> List.map(fun (x,y) -> x * y)
  let num = sumlist lst2
  let denom = sumlist weights 
  num/denom

(* Question 2. *) (* Do not edit this line. *)

let rec memberof pair =
  match pair with
  |(x, []) -> false
  |(x, y::ys) -> if (x = y) then true else memberof(x, ys)

let rec remove(item, lst) =
  match lst with
  | [] -> []
  | x::xs -> if item <> x then x::remove(item, xs) else remove(item, xs)  

(* Question 3. *) (* Do not edit this line. *)

let findMax l =
  let rec helper(l,m) = 
    match l with
    | [] -> m
    |(x::xs) -> if m < x then helper(xs, x) else helper(xs, m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)     

(* Question 4. *) (* Do not edit this line. *)

let rec selsort l =         
  match l with
  | [] -> []
  | x::xs ->
    let max = findMax(l)
    max::selsort(remove(max, l)) 

(* Question 5. *) (* Do not edit this line. *)

let rec common twolists =
  match twolists with
  | ([], []) -> []
  | (x::xs, []) -> []
  | ([], y::ys) -> [] 
  | (x::xs, ys) ->
    if memberof(x, ys) then x::common(remove(x,xs), ys)
    else common(remove(x, xs), ys)

(* Question 6. *) (* Do not edit this line. *)
let rec split l =                                
  match l with                                  //split returns pair of lists, and we want to get them seperately
  | x1::x2::xs ->                               //pattern match on answer returned by recursive call
    let (l1, l2) = split xs                     // l1 and l2 packaged together in a pair
    (x1::l1, x2::l2)                            //take the piece apart and match them correspondingly
  | [] -> ([], [])
  | [x] -> ([x], [])   

let rec merge twolists =
  match twolists with
  | ([], []) -> []
  | (l1, []) -> l1
  | ([], l2) -> l2
  | (x::xs, y::ys) -> if x < y then x::merge(xs, y::ys) else y::merge(x::xs, ys) 


let rec mergesort l =
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns ->
    let(l1, l2) = split l
    merge (mergesort l1, mergesort l2) 
