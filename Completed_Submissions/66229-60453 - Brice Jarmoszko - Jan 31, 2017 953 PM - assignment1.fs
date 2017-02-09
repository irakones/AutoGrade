(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Brice Jarmoszko, Id Number: 260515287 *) (* Edit this line. *)

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

let rec sumlist (l: float list ) = 
    match l with 
    | [] -> 0.0
    | x::xs -> x+sumlist(xs)


let rec pairlists twolists =
    match twolists with
    | ([],[]) -> []
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"

(* 'a list * 'b list -> ('a * 'b) list is the wrong type. When using the correct type it helps to use the function pairlists. *)

let w_mean weights data =
  let denom = sumlist weights
  let pairs = pairlists (weights, data)
  (sumlist (List.map (fun (x,y) -> x * y) pairs))/denom
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (_,[]) -> false
    | (x,y::ys) -> if x=y then true else memberof(x,ys)


let rec remove(item, lst) = 
    match (item, lst) with
    | (_,[]) -> []
    | (x,y::ys) -> if x=y then remove(x,ys) else y::remove(x,ys)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match (l,m) with
    | ([],y) -> y
    | (x::xs,y) -> if y > x then helper(xs,y) else helper(xs,x)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with 
    | [] -> []
    | x -> findMax(x)::selsort(remove(findMax(x),x))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with
    | ([],[]) -> []
    | (x::xs, y) -> if memberof(x,y) then x::common(xs,y) else common(xs,y)
    | ([],x::xs) -> []

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
    let rec helper l (xs, ys) =
        match l with
        | x::y::rest -> helper rest (x::xs, y::ys)
        | [x] -> (x::xs, ys)
        | _ -> (xs, ys)
    helper l ([], [])
            

let rec merge twolists = 
    match twolists with 
    | ([],[]) -> []
    | (x::xs, y::ys) -> if (x < y) then x::merge(xs,y::ys) else y::merge(ys,x::xs)
    | ([],x::xs) -> x::merge([],xs)
    | (x::xs,[]) -> x::merge([],xs)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n -> let (a, b) = split n
         merge (mergesort(a),mergesort(b))


