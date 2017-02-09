(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Alan Healey-Greene, Id Number: 260631156 *) (* Edit this line. *)

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

let rec sumlist l: float = //failwith "Not implemented"  
  match l with 
  | [] -> 0.0
  | x :: xs -> 
              let sum = sumlist(xs)
              (x + sum)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)  //failwith "Not implemented"


let w_mean weights data =  //failwith "Not implemented"
  let temp = pairlists(weights,data)
  let temporary = temp |> List.map(fun(x,y) -> (x*y))
  let weightedMean = (sumlist temporary) / (sumlist weights)
  weightedMean


  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = //failwith "Not implemented"
  match pair with
  | (i, y::ys) -> (i=y) || (memberof (i, ys))
  | (i,[]) -> false


let rec remove(item, lst) = //failwith "Not implemented"
  match lst with
  | [item] -> []
  | x::xs -> if x = item then remove(x,xs)
             else 
                  x::remove(item, xs)



(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = //failwith "Not implemented"
    match l with
    | [] -> m
    | x::xs -> if x > m then helper(xs, x)
               else helper (xs, m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = //failwith "Not implemented"
    match l with
    | [] -> []
    | x::xs -> let max = findMax(l)
               let templist = remove(max,l)
               max :: selsort(templist)

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = //failwith "Not implemented"
  match twolists with 
  |([],[]) -> []
  |([],x::xs) -> []
  |(x::xs,[]) -> []
  |(x::xs, y::ys) -> if memberof(x, y::ys) then x::common(xs,ys)
                     else common(xs,ys)

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