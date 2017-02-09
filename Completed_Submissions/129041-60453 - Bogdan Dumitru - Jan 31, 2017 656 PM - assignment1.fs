(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Bogdan Dumitru, Id Number: 260690446 *) (* Edit this line. *)

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
module hw1_sol
let rec sumlist l = 
  match l with
    | [] -> 0.0
    | x::xs -> (x + (sumlist xs))

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys) //build a list of pairs (weight, data)
        

let w_mean weights data =  
  let z = List.map(function (x,y) -> x*y) (pairlists (weights, data)) in sumlist z/sumlist weights
  //use multiply pairs made by pairlists function, sum the answers and divide byt sum of weights

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
    | (_, []) -> false
    | (x, y::ys) ->
        if x = y then true
        else memberof (x, ys)

let rec remove(item, lst) = 
  match (item, lst) with
    | (_, []) -> []
    | (x, y::ys) ->
        if x = y then ys
        else y::remove(x, ys)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
      | [] -> m
      | x::xs ->
          if x > m then helper(xs,x)
          else helper (xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  match l with
    | [] -> []
    | [x] -> [x]
    | (x::xs) as L->
      let max = findMax L
      //Check if there are duplicates to remove
      if max = findMax (remove (max, L)) then
        selsort(remove(max, L)) //Duplicate found, redo iteration with duplicate removed
      else
        max::selsort(x::remove (max, xs)) //Duplicate not found, continue selection sort of rest of list

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with 
    | ([], []) -> []
    | (x::xs, []) -> []
    | ([], y::ys) -> []
    | (x::xs, y::ys) ->
      if x=y || memberof (x, ys) then x:: common (xs, y::ys) //Common element found, put it in returned list
      else common (xs, y::ys) //No common element found, continue to next iteration but with front element of first list removed

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  match l with
    | [] -> ([],[])
    | [x] -> ([x], [])
    | x::y::xs ->
       let (leftList, rightList) = split xs in (x::leftList, y::rightList) 


let rec merge twolists = 
 match twolists with
  | ([],[]) -> []
  | (l,[]) -> l
  | ([], l) -> l
  | (x::xs, y::ys) ->
  //If-else handles sorting on when merging
    if x < y then
      x::merge(y::ys,xs)
    else
      y::merge(x::xs,ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns ->
    let (ll, rl) = split l in merge (mergesort ll, mergesort rl)


