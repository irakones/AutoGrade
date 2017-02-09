(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Richard Lei, Id Number: 260690500 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)

(* module hw1_sol.  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)

let rec sumlist l = 
  match l with
    | [] -> 0.0
    | (x:float::xs) -> x+sumlist(xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> ((x,y)::pairlists(xs,ys))

let w_mean weights data =  
  let weightdatalist = pairlists(weights,data) |> List.map (fun (x,y) -> (x*y))
  let sumweightdata = sumlist weightdatalist
  let sumweights = sumlist weights
  sumweightdata/sumweights

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
    |(a,[]) -> false
    |(a,x::xs) -> 
      if a = x then true
      else memberof(a,xs)

let rec remove(item, lst) =  
  match lst with
    |[] -> []
    |(x::xs) -> 
      if x = item then remove(item,xs)
      else x::remove(item,xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
      |[] -> m
      |(x::xs) -> 
        if x>m then helper(xs,x)
        else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  let rec removedupes(l) =
    match l with
      | [] -> []
      | x::xs -> 
        if memberof(x,xs) then x::removedupes(remove(x,xs))
        else x::removedupes(xs)
  let rec maxsort(l) =
    match l with 
      | [] -> []
      | x::xs -> findMax(x::xs)::maxsort(remove(findMax(x::xs),x::xs))
  let sortedlist = maxsort(removedupes(l))
  sortedlist

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> []
    | (x::xs,[]) -> []
    | (x::xs,y::ys) -> 
      if memberof(x,y::ys) then x::common(xs,y::ys)
      else common(xs,y::ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  let l1 = []
  let l2 = []
  let rec helper(startlist,listone,listtwo) = 
    match startlist with
      | [] -> (listone,listtwo)
      | x::[] -> (x::listone,listtwo)
      | (x1::x2::xs) -> helper(xs,x1::listone,x2::listtwo)
  helper(l,l1,l2)

let rec merge twolists = 
  match twolists with
    | ([],[]) -> []
    | (x,[]) -> x
    | ([],x) -> x
    | (x::xs,y::ys) -> 
      if x<y then x::merge(xs,y::ys)
      else y::merge(x::xs,ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (l1,l2) = split(l)
    merge(mergesort(l1),mergesort(l2))
