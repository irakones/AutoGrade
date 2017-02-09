(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Miranda LaBrash, Id Number: 260631836 *) 

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

(* sumlist computes sum of input float list l, returns sum value as float *)
let rec sumlist l = 
  match l with
  | [] -> 0.0
  | x :: xs ->
    let s = sumlist(xs)
    (x + s)
let testTotal = sumlist [1.0; 2.5; 100000.0] 


let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y) :: pairlists(xs, ys)
  
let realsList = [1.9; 2.6], [4.3; 5.2]


let w_mean weights data = 
  
  let outputFunction realsList finalMean =

    let sumOfWeights = sumlist  weights 
    let newList = pairlists( weights, realsList )
    let weightedList = List.map (fun (x, y) -> (x*y) ) newList
     
    let sumOfProducts = sumlist weightedList 
    finalMean = (sumOfProducts / sumOfWeights)
    
  in outputFunction


(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = failwith "Not implemented"


let rec remove(item, lst) = failwith "Not implemented"


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = failwith "Not implemented"

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = failwith "Not implemented"

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = failwith "Not implemented"

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)


let rec merge twolists =
    match twolists with
    | ([], y ) -> y
    | ( x, []) -> x
    | ( x :: xs, y :: ys )  ->
        if x > y then y :: merge (xs, ys)
        else
        x :: merge (xs, ys )

let rec split l = 
    let rec helper l2 se1 se2 =
        match l2 with
        | [] -> (se1, se2)
        | [x] -> (x :: se1, se2)
        | x :: y :: xs -> helper xs (x :: se1) (y :: se2)
    in helper l [][]

let rec mergesort l = 
    match l with
    | [] -> []
    | (x::[]) -> x::[] (* Without this you will go into an infinite loop. *)
    |  x::xs -> let (l1, l2) = split l
                in merge ( mergesort l1 ) (mergesort l2)
