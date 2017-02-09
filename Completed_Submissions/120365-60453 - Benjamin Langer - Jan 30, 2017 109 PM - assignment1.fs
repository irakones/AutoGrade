(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Benjamin Langer, Id Number: 260680875 *)
(* module hw1_sol. Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)



let rec sumlist l =
  match l with
  | [] -> 0.0
  | x::xs -> x + sumlist xs

let rec pairlists twolists =                  
  match twolists with
  | ([], []) -> []
  | ([],x::xs) -> failwith "Error -- lists are not of the same length"
  | (x::xs, []) -> failwith "Error -- lists are not of the same length"
  | (x::xs, y::ys) -> (x,y)::pairlists(xs, ys)  

let w_mean weights data =
  let list = pairlists(weights, data)
  let list2 = list |> List.map(fun (x,y) -> x * y)
  let res = (sumlist list2)/(sumlist weights)
  res



(* Question 2. *) (* Do not edit this line. *)

let rec memberof element =
  match element with
  |(x, []) -> false
  |(x, y::ys) -> if (x = y) then true else memberof(x, ys)

 
let rec remove (element,list) =
  match list with
  | [] -> []
  | x::xs -> if (x=element) then remove(element, xs) else x::remove(element, xs)



(* Question 3. *) (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l with
    | [] -> m
    | (x::xs) -> if x>m then helper(xs,x) else helper(xs,m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)   

(* Question 4. *)  (* Do not edit this line. *)

let rec selsort l =
    match l with
    | [] ->[]
    | l -> findMax(l)::selsort(remove(findMax(l),l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common (list1,list2) =
    match (list1,list2) with
    | (list1,[]) -> list1
    | (list1, x::xs) -> common((x::remove(x,list1)),xs)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let rec split l =
    match l with
    | [] -> ([], [])
    | [l] -> ([l], [])
    | first::second::rem -> let (x,y) = split rem in (first::x, second::y)

let rec merge (list1,list2) = 
    match (list1,list2) with
    | (list1, []) -> list1
    | ([], list2) -> list2
    | (x::xs, y::ys) -> if x < y then x :: merge (xs, list2) else y :: merge (list1, ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (l::[]) -> l::[] (* Without this you will go into an infinite loop. *)
  | l -> let (x,y) = split(l) in merge(mergesort(x), mergesort(y))


  