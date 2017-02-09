(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Ashvitha Sridharan, Id Number: 260665014 *) (* Edit this line. *)

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
  | [] -> float 0
  | x::xs -> x+(sumlist xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::(pairlists (xs,ys))


let w_mean weights data =  
  let weighted = pairlists (weights,data) |> List.map (fun (x, y) -> x * y)
  (sumlist weighted / sumlist weights)
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with 
    | (x,[]) -> false
    | (x, y::ys) -> if x=y then true else memberof (x,ys)

let rec remove(item, lst) = 
  let test = []
  match lst with
    | [] -> []
    | (x::xs) when x=item -> remove(item,xs)
    | (x::xs) -> x::remove(item,xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match (l,m) with 
    | ([],x) -> x
    | (y::ys,x) -> if x>y then helper (ys,x) else helper (ys,y) 

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  //find the largest value in l
  match l with
    | [] -> []
    | (x::xs) -> findMax l::selsort(remove(findMax l,(x::xs)))

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
  match twolists with 
    | ([],[]) -> []
    | (x::xs,[]) -> []
    | ([],y::ys) -> []
    | (x::xs,y::ys) when memberof(x,y::ys) -> x::common(xs,remove(x,y::ys))
    | (x::xs,y::ys) -> common (xs, y::ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  let rec helper lst l1 l2 = 
    match lst with 
      | [] -> (l1,l2)
      | (x::xs) -> helper xs (l2) (x::l1)
  helper l [] []

let rec merge twolists = 
    match twolists with
      |([],[]) -> []
      |(x::xs, []) -> x::xs
      |([], y::ys) -> y::ys
      |(x::xs,y::ys) -> 
        if (x<y) then x::merge(xs,y::ys) 
        else y::merge(ys, x::xs)

let rec mergesort l = 
  match l with
    | [] -> []
    | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
    | n::ns -> 
      let (l1,l2) = split l
      merge (mergesort l1, mergesort l2)