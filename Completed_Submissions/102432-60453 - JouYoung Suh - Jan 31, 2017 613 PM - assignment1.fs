(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: JouYoung Suh, Id Number: 260627148 *) (* Edit this line. *)

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
  | x::xs -> x + sumlist(xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> [x,y]::pairlists (xs,ys)

let w_mean weights data =  
  let rec productlists(l1, l2) =
    match (l1, l2) with
      | ([],[]) -> []
      | ([],x::xs) -> failwith "Error -- lists are not of the same length"
      | (x::xs, []) -> failwith "Error -- lists are not of the same length"
      | (x::xs, y::ys) -> x * y + 0.0::productlists (xs, ys)
  let t1 = productlists(weights, data)
  let t2 = sumlist(t1)
  let b = sumlist(weights)
  t2/b
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (y, []) -> false
    | (y, x::xs) -> if (y = x) then true else memberof(y, xs)
   

let rec remove(item, lst) = 
    match lst with
    | [] -> []
    | x::xs -> if (x = item) then remove(item, xs) else x::remove(item, xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l with
    | [] -> m
    | [x] -> x
    | x1::x2::xs -> 
        if (x1>x2) 
            then helper(x1::xs, m)
            else helper(x2::xs, m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | [] -> []
    | x::xs -> 
        let max = findMax (x::xs)
        let restList = remove(max, x::xs)
        let sortedList = selsort(restList)
        max::sortedList

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    let emptylst = []
    match twolists with
    | (xs,[]) -> []
    | ([],ys) -> []
    | (x::xs, ys) ->
        if(memberof(x,ys)) then x::common (xs,ys) else common (xs,ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with
    | [] -> [],[]
    | [x] -> [x], []
    | (x::y::xs) ->
        let (odd, evens) = split(xs) in (x::odd, y::evens) 
let rec merge twolists = 
    match twolists with
    | l, [] -> l
    | [], l -> l
    | x::xs, y::ys -> 
        if (x < y) then x::merge(xs, y::ys)
        else y::merge(x::xs,ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns ->
    let (a,b) = split(l)
    let l1 = mergesort(a)
    let l2 = mergesort(b)
    merge(l1, l2)



