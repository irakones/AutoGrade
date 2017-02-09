(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Ginikachukwu Ume-Onyido, Id Number: 260675844 *) (* Edit this line. *)

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
    | x::xs -> x + sumlist xs //recursively sum list 

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: pairlists (xs,ys) //create new list with matched pair placed in the beginning of rest of two old lists

let w_mean weights data =  failwith "Not implemented"
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
  | (a, []) -> false
  | (a, x::xs) -> 
    if (a = x) then true
    else memberof (a, xs)

let rec remove(item, lst) = 
  match (item, lst) with
  | (a, []) -> []
  | (a, x::xs) ->
    if (a <> x) then x :: remove(a, xs)
    else remove(a, xs) 

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match (l,m) with
    | ([],a) -> a
    | (x::xs,a) ->
      if (a > x) then helper(xs,a)
      else helper(xs,x)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  match l with
  | [] -> []
  | (x::xs) ->
    let m = findMax(x::xs)
    let rest = remove(m, x::xs)
    m :: selsort rest 

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
  | ([],[]) -> []
  | (x::xs,[]) -> []
  | ([],y::ys) -> []
  | (x::xs,y::ys) ->
    if (memberof (x,y::ys)) then x :: common (xs,y::ys)
    else common (xs,y::ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  let rec helper original sub1 sub2 =
    match original with 
      | [] -> (sub1, sub2) 
      | [x] -> (x::sub1, sub2)
      | x::y::xs -> 
      helper xs (x::sub1) (y::sub2)
  helper l [] []

let rec merge twolists = 
  match twolists with 
  | (x, []) -> x
  | ([], y) -> y
  | (x::xs, y::ys) -> 
    if (x<=y) then selsort (x:: merge (remove(x,xs), y::ys))
    else selsort (y::merge (x::xs, ys))

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (sub1,sub2) = split l
             merge ((mergesort sub1), (mergesort sub2))









