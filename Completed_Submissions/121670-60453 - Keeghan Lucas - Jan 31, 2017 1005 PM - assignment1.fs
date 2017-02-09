(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Keeghan A Lucas, Id Number: 260657556 *) (* Edit this line. *)

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
  |  x::xs -> x + sumlist xs 

let rec pairlists twolists =
  match twolists with
  | ([],[]) -> []
  | ([],x::xs) -> failwith "Error -- lists are not of the same length"
  | (x::xs, []) -> failwith "Error -- lists are not of the same length"
  | (x::xs, y::ys) -> (x,y) :: pairlists (xs,ys)

let rec assist realWeights  = (*Assist method to multiply the pairs*)
    match realWeights with 
    | [] -> 0.0
    | (x,y)::zs -> (x*y) + assist zs 

let w_mean  weights data =  (assist (pairlists(weights, data)))/ sumlist weights
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with 
  | (x,[]) -> false
  | (x, y::ys) -> if x = y then true else memberof (x, ys)

let rec remove(item, lst) = 
  match (item,lst) with 
  | (x,[]) -> []
  | (x, y::ys) -> if x = y then remove(x, ys) else y::remove(x,ys)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with 
    | [] -> m
    | x::xs -> if x > m then helper(xs,x) else helper(xs,m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  match l with 
  | []-> []
  |_ -> findMax l::selsort(remove(findMax l ,l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with 
  | ([],[]) -> []
  | ([],x::xs) -> []
  | (x::xs, []) -> []
  | (x::xs, ys) -> if memberof(x,ys) then x:: common(xs, remove(x,ys)) else common(xs,ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  let rec splithelp l (splitone,splittwo) = 
    match l with 
    | [] -> (splitone,splittwo)
    | [x] -> (x::splitone, splittwo)
    | a::b::cs -> splithelp cs (a::splitone, b::splittwo) 
  splithelp l ([],[])

let rec merge twolists = 
  match twolists with 
  | ([],[]) -> []
  | ([],x::xs) -> x::merge ([], xs)
  | (x::xs, []) -> x::merge (xs,[])
  | (x::xs, y::ys) ->  if x > y then y::merge(x::xs,ys) else x::merge(xs,y::ys)

let rec mergesort l =
  let mergesorthelper k = (*allows for recursive call on two lists*)
     let (l1,l2) = split k 
     merge(mergesort l1, mergesort l2)
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> mergesorthelper (n::ns)






