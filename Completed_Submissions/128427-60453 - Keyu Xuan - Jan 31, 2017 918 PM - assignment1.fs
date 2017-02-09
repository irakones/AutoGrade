(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Keyu Xuan , Id Number: 260676756 *) 



(* Question 1 *) (* Do not edit this line. *)

let rec sumlist l = 
  match l with 
  |[] -> 0.0
  |x :: xs -> x + (sumlist xs)

let rec pairlists twolists =
   match twolists with
   | ([],[]) -> []
   | ([],x::xs) -> failwith "Error -- lists are not of the same length"
   | (x::xs, []) -> failwith "Error -- lists are not of the same length"
   | (x::xs, y::ys) -> (x,y):: (pairlists (xs,ys))

let rec helpf li  =
  match li with
  |[] -> 0.0
  |x :: xs -> 
    let (n,p)=x in
    (n*p) + helpf xs

let w_mean weights data =  
  let pair = pairlists (weights,data)
  let deno = sumlist weights
  let nume = helpf pair 
  nume/deno
 
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  let (item,lis) = pair
  match lis with
  |x :: xs -> if (item=x) then true else memberof (item,xs)
  |[] -> false

let rec remove(item, lst) = 
  match lst with
  |x :: xs -> if (item=x) then remove (item,xs) else x::remove (item,xs)
  |[] -> []


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with  
    |[] -> m
    |x ::xs -> if (x>m) then helper(xs,x) else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let findMaxx l = 
  let rec helper(l,m) = 
    match l with  
     |[] -> m
     |x ::xs -> if (x>m) then helper(xs,x) else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)


let rec remDup lis = 
  let rec helper num li = 
    match li with
    |[] -> true
    |x :: xs -> if (x=num) then false else (helper num xs)

  match lis with
  |[] -> []
  |x :: xs ->
    let v = helper x xs
    if v then x :: (remDup xs) else (remDup xs)

let rec rem num lit =
  match lit with
  |[] -> []
  |x :: xs -> if num=x then (rem num xs) else x::(rem num xs)

let rec selsort l = 
  let l1 = remDup l
  match l1 with
  |[] -> []
  |_ ->
    let big = findMaxx l1
    big :: selsort (rem big l1)

(* Question 5. *)  (* Do not edit this line. *)

let rec memberoff pair = 
  let (item,lis) = pair
  match lis with
  |x :: xs -> if (item=x) then true else memberoff (item,xs)
  |[] -> false

let rec remDupp lis = 
  let rec helper num li = 
    match li with
    |[] -> true
    |x :: xs -> if (x=num) then false else (helper num xs)

  match lis with
  |[] -> []
  |x :: xs ->
    let v = helper x xs
    if v then x :: (remDupp xs) else (remDupp xs)

let rec common twolists = 
  let (l,r) = twolists
  match l with
  |[]-> []
  |x :: xs ->
    let va = memberoff (x,r)
    if va then 
      let v1 = x :: (common (xs,r)) 
      remDupp v1
    else 
      let v2 = common (xs,r)
      remDupp v2

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  match l with
  |[] -> ([],[])
  |x :: []->([x],[])
  |x1 :: x2 :: xs -> 
    let (s,p) = split xs
    (x1::s,x2::p)

let rec merge twolists = 
  let (a,b) = twolists
  match (a,b) with

  |(a,[]) ->a
  |([],b) ->b
  |(x::xs,y::ys) ->
    if (x>y) then y::merge (x::xs,ys)
    else x::merge (xs,y::ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (s,p) = split l
    let v = mergesort s
    let w = mergesort p
    merge (v,w)