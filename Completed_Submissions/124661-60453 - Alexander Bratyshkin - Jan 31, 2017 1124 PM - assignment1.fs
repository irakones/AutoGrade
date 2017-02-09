(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Alexander Bratyshkin, Id Number: 260684228 *) (* Edit this line. *)

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

let l1 = [1.0;1.0;1.0]
let l2 = [2.0;4.0;3.0]

let l3 = [2.0;4.0;1.0;8.0;61.0;9.0;9.0;1.0;3.0;6.0;0.0;-4.3]  

let rec sumlist l =
  match l with
  | [] -> 0.0
  | x::xs -> x + (sumlist xs)

let rec pairlists(l1, l2) = 
    match l1, l2  with 
    | ([], []) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> [(x, y)]@pairlists(xs, ys) 

let w_mean weights data =  
    let numeratorlist = List.map (function (x,y) -> x * y) (pairlists(weights, data))
    (sumlist numeratorlist)/(sumlist weights)

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
    match pair with
    | (x, []) -> false
    | (x, y::ys) -> if (x = y) then 
                     true 
                    else 
                     memberof(x, ys)
let rec remove(item, lst) = 
    match lst with 
    | [] -> []
    | x::xs -> if (item <> x) then 
                x::remove(item, xs) 
               else 
                remove(item, xs) 

(* Question 3. *)  (* Do not edit this line. *)

let findMax l =
  let rec helper(l,m) = 
    match l with
    | [] -> m
    |(x::xs) -> if x < m then 
                 helper(xs, m) 
                else 
                 helper(xs, x)               
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)     

(* Question 4. *)  (* Do not edit this line. *)

let rec selsort l = 
    match l with
    | [] -> []
    | [x] -> [x] 
    | x::xs ->  let max = findMax(l)  
                max::selsort(remove(max, l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  let rec helper (l, m) = 
    match l with
        | ([], []) -> m 
        | ([], x::xs) -> m
        | (x::xs, []) -> m
        | (x::xs, y::ys) ->  if memberof(x,y::ys) then 
                              helper((xs, y::ys), m@[x]) 
                             else 
                              helper((xs, y::ys), m)
  helper(twolists, [])

let selsorttest = selsort([1..2..25]@[1..5..21]) 

(* Question 6. *)   (* Do not edit this line. *)

let rec split l= 
  match l with 
  | [] -> ([], [])
  | [x] -> ([x], [])
  | x::y::rest -> match split(rest) with 
                  | (a, b) -> (x::a, y::b)

let rec merge twolists = 
  match twolists with 
  | ( [], ys ) -> ys
  | ( xs, [] ) -> xs
  | ( x:: xr, y::yr) -> if (x < y) then 
                         x::merge(xr, y::yr)
                        else
                         y::merge(x::xr, yr) 
  
let rec mergesort l = 
  match l with
  | [] -> []  
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (leftlist, rightlist) = split l 
             merge ((mergesort leftlist, mergesort rightlist))

