(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Ethan Cohn, Id Number: 260620210 *) (* Edit this line. *)

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
    | x::xs -> x + sumlist xs

//sumlist[4.5;3.6;2.6;1.0];;

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y)::pairlists (xs, ys)

//pairlists([3.5;6.2;7.9], [5.0;3.0;7.5]);;


let w_mean weights data = 
  let pair = pairlists (weights, data)
  let rec mult a = 
    match a with
    | [] -> []
    | (x,y)::xs -> x*y::mult xs
  sumlist (mult pair) / sumlist weights

//w_mean [3.5;6.2;7.9] [5.0;3.0;7.5]

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with 
  | (x, []) -> false
  | (x, y::ys) -> 
    if x = y then 
      true
    else
      memberof (x, ys)

//memberof (4, [5;3;6]);;
//memberof (4, [4;5;6]);;

let rec remove (item, list) =
  match (item, list) with 
  | (x, []) -> []
  | (x, y::ys) -> 
    if x = y then 
      remove(x, ys)
    else
       y::remove(x, ys)

//remove (4, [4;5;6]);;

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | x::xs -> helper(xs, max m x)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

//findMax [5;4;6;3;1;6;66;77;8];;

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =        
  match l with
  | [] -> []
  | xs ->  findMax xs::(selsort (remove (findMax xs, xs)))

//selsort ([1..2..25]@[1..5..21]);;

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with 
  | ([],[]) -> []
  | ([],ys) -> []
  | (xs,[]) -> []
  | (x::xs,ys) -> 
    if memberof (x,ys) then
      x::common(remove(x,xs),ys)
    else
      common(remove(x,xs),ys)

//common ([3;4;5;7;2],[1;3;5;7;9;1]);;

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  let rec helper (first, slow, fast) =
    match (first, slow, fast) with 
    | (first, ys, ([] | [_])) -> (first, ys)
    | (_, [], _) -> ([],[])
    | (first, x::xs, y::y'::ys) -> helper(x::first, xs, ys)
  helper ([], l, l)
    
    
let rec merge twolists = 
  match twolists with
  | (xs, []) -> xs
  | ([], ys) -> ys
  | (x::xs, y::ys) -> 
    if (x < y) then
      x :: merge (xs, y::ys)
    else 
      y :: merge (x::xs, ys) 


let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (a, b) = split l
    merge (mergesort a, mergesort b)
  
//mergesort [15 .. -2 .. 1];;
//split [15 .. -2 .. 1];;