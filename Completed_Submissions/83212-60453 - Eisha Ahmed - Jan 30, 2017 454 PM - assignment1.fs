(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Eisha Ahmed, Id Number: 260583530 *)

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
    | head :: tail -> head + sumlist tail
    | [] -> 0.0

let rec pairlists twolists =
  match twolists with
  | ([],[]) -> []
  | ([],x::xs) -> failwith "Error -- lists are not of the same length"
  | (x::xs, []) -> failwith "Error -- lists are not of the same length"
  | (x::xs, y::ys) -> (x,y) :: pairlists (xs,ys)

let w_mean weights data =
    let rec multPairList list =
        match list with
        | (a,b)::tail -> a*b :: multPairList tail
        | _ -> []
    ( pairlists (weights, data) |> multPairList |> sumlist ) / (sumlist weights)

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
    match pair with
    | (_,[]) -> false
    | (x,y::ys) when y=x -> true
    | (x,y::ys) -> memberof (x,ys)

let rec remove (item, lst) =
    match lst with
    | [] -> []
    | x::xs when x=item -> remove (item, xs)
    | x::xs -> x :: remove (item, xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l with
    | [] -> m
    | x::xs when x > m -> helper(xs,x)
    | x::xs -> helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
    match l with
    | [] -> []
    | x::xs ->
        let m = findMax l
        m :: selsort (remove (m,l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  match twolists with
    | ([],[]) -> []
    | ([],y::ys) -> []
    | (x::xs, []) -> []
    | (x::xs, y::ys) when memberof (x,y::ys) -> x :: common (remove(x,xs), remove(x,y::ys))
    | (x::xs, y::ys) -> common (xs, y::ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
    match l with
    | [] -> ([],[])     // matches empty imput
    | [x] -> ([x],[])   // matches only one list item
    | x::y::t ->
        let n = split t
        (x::(fst n),y::(snd n))

let rec merge twolists =
    match twolists with
    | ([],[]) -> []
    | ([],y::ys) -> y::ys
    | (x::xs,[]) -> x::xs
    | (x::xs,y::ys) when x >= y -> x :: merge (xs,y::ys)
    | (x::xs,y::ys) -> y :: merge (x::xs,ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | [n] -> [n] (* Without this you will go into an infinite loop. *)
  | n::ns -> merge ( mergesort(fst (split l)), mergesort(snd (split l)) )
