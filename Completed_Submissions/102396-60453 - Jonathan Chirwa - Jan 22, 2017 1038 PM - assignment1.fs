(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Jonathan Chirwa Id Number: 260610437 *) (* Edit this line. *)

(* Question 1 *) (* Do not edit this line. *)
let rec sumlist l = 
(*helper method takes 'a and 'a list, sums 'a with all contents of 'a list*)
    let rec helper y li =
        match li with
        |[] -> y
        |x::xs -> helper (x+y) xs
    helper 0.0 l

let rec pairlists twolists =
    match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> x::xs
    | (x::xs, []) -> x::xs
    | (x::xs, y::ys) -> x*y :: pairlists (xs,ys)

let w_mean weights data = 
    sumlist (pairlists (weights,data)) / sumlist weights
  
(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair =
    match pair with
    | (a,[]) -> false
    | (a,x::xs) when a=x -> true
    | (a,x::xs) -> a=x || memberof (a,xs)

let rec remove(item, lst) = 
    match (item,lst) with
    |(item, []) -> []
    |(item,x::xs) when item = x -> remove(item,xs)
    |(item, x::xs) -> x::remove(item,xs)

(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
  let rec helper (li,m) = 
    match (li,m) with
    |([], m) -> m
    |(x::xs, m) when x > m -> helper (xs,x)
    |(x::xs, m) when x <= m -> helper (xs,m)
    |(_,_)-> failwith "findMax helper failed"
  match l with
  | [] -> failwith "findMax failed"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l =  
  let rec max (lst,n) =
    match lst with
    |[]->n
    |x::xs when x <= n -> max (xs,n)
    |x::xs when x > n -> max (xs,x)
    |_ -> failwith " max helper for selsort failed"
  let buildlist (li,k) =
    let n = max (li,k)
    let lz = remove (n,k::li)
    n::selsort lz
  match l with
  |[] -> []
  |(x::xs)-> buildlist (xs,x)

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
    let (s,p) = twolists
    match s with
    |[] -> []
    |x::xs when memberof (x,p) ->
                                let li = remove (x,xs)
                                x::common (li,p)
    |x::xs -> common (xs,p)

(* Question 6. *)   (* Do not edit this line. *)
(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let rec split l =
    match l with
    |[]->([],[])
    |x::xs -> 
            let a=x
            match xs with
            |[]->([a],[])
            |y::ys->
                let (l1,l2)= split ys
                (a::l1,y::l2)

let rec merge twolists = 
    let (l1,l2)= twolists
    match (l1,l2) with
    |([],[])->[]
    |(x::xs,[])->x::merge (xs,[])
    |([],y::ys)->y::merge ([],ys)
    |(x::xs,y::ys) when x<=y-> x::merge (xs,y::ys)
    |(x::xs,y::ys) when x>y ->y::merge (x::xs,ys)
    |(_,_)-> failwith "merge twolists failed"

let rec mergesort l = 
    match l with
    | [] -> []
    | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
    | n::ns -> 
        let (l1,l2)= split (n::ns)
        merge (mergesort l1,mergesort l2)

let d= [2.0;7.0;4.0;9.0;1.0;4.0;12.0;13.0;15.0]
let num = mergesort d