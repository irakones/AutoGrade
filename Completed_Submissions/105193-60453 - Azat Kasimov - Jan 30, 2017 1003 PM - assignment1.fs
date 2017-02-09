(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Azat Kasimov, Id Number: 260609792 *) (* Edit this line. *)

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

let rec sumlist l:float = //failwith "Not implemented"
    match l with 
    | [] -> 0.00
    | x::xs ->
            let sum = sumlist(xs)
            x+sum


let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> ((x,y)::pairlists(xs,ys))


let rec weightedval inputs =
        match inputs with
            | ([],[]) -> []
            | ([],x:float::xs) -> failwith "Error -- lists are not of the same length"
            | (x::xs, []) -> failwith "Error -- lists are not of the same length"
            | (x::xs, y::ys) -> (x*y::weightedval(xs,ys))




let w_mean weights data = //failwith "Not implemented"
    let w:float list = weightedval(weights,data)
    (sumlist w)/(sumlist weights)


   

    
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = //failwith "Not implemented"
    match pair with
        | (x,y::ys) -> if x=y then true else memberof (x,ys)
        | (x,[]) -> false

let rec remove(item, lst) = //failwith "Not implemented"
    match lst with
        | [y] when y=item -> []
        | [] -> []
        | y::ys ->  if y=item then remove(item,ys) else y::remove(item,ys)



(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = //failwith "Not implemented"
    match l with
        | x::xs when x>m -> helper (xs,x)
        | x::xs when x<m -> helper (xs,m)
        | [] -> m

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)

let rec selsort l = //failwith "Not implemented"
    match l with
    | [] -> []
    | x::xs -> let m=findMax(l) 
               m::selsort(remove(m,l))



(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = //failwith "Not implemented"
    match twolists with
    | ([],[]) -> []
    | ([],y) -> []
    | (x,[]) -> []
    | (x::xs,y) when memberof(x,y) -> x::common(xs,remove(x,y))
    | (x::xs,y) when not (memberof(x,y)) -> common(xs,y)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)


let rec split l= //failwith "Not implemented"
    match l with
    | [] -> ([],[])
    | [x] -> ([x],[])
    | x::y::xs -> let (l1,l2)= split(xs)
                  (x::l1, y::l2)


let rec merge twolists = //failwith "Not implemented"
    match twolists with
    | ([],y) -> y
    | (x,[]) -> x
    | ([],[]) -> ([]) 
    | (x::xs,y::ys) when x=y -> x::y::merge(xs,ys)
    | (x::xs,y::ys) when x<y -> x::merge(xs,y::ys)
    | (x::xs,y::ys) when y<x -> y::merge(x::xs,ys)



let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (M,N) = split (n::ns)
             merge(mergesort M, mergesort N)
