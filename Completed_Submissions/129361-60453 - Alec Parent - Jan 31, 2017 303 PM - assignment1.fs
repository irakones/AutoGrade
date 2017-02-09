(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Alec Parent, Id Number: 260688035 *) (* Edit this line. *)

module Comp302A1

(* Question 1 *) (* Do not edit this line. *)

let rec sumlist l =          (* Take the first value of a list and add it to the recursive call. *)
  match l with
  | [] -> 0.0
  | x::xs -> x + sumlist xs

let rec pairlists twolists =     (* Take the first element of each list and pair them together in a tuple, recursively call on *)
  match twolists with            (* next element. *)
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)

(* New method: multtup takes a list of comparable type tuples and creates a new list, 
   where entry n is the product of both elements in the nth tuple of the original list. *)
let rec multtup tuplist = 
  match tuplist with
  | [] -> []
  | x::xs ->
    let z = fst x * snd x
    z::multtup xs

let w_mean weights data =   (* Calculation for weighted mean using the previously defined recursive functions. *)
  ( sumlist ( multtup ( pairlists(data,weights) ) ) ) / ( sumlist weights )
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = (* Compare m with the first element in the list. True if equal, recursively call on shorter list if not. *)
  match pair with       (* Return false once the list is exhausted. *)
  | (m,[]) -> false
  | (m,x::xs) -> 
    if (x=m) then true
    else memberof (m,xs)
  
let rec remove(item, lst) = (* Compare the first element of lst with item. Recursively call remove on the rest of the list if they *)
  match lst with            (* are equal. Append it to the front and recursively call remove on the rest of the list otherwise. *)
  | [] -> []
  | hd::tl when hd = item -> remove(item,tl)
  | hd::tl -> hd::(remove(item,tl))

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = (* Take the first element m of the list and set it as the original max, compare it to the second element x. *)
    match l with        (* set whichever is larger as the new max and recursively call helper on the rest of the list. *)
    | [] -> m
    | (x::xs) -> 
      if (x>m) then helper(xs,x)
      else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  match l with   (* Find the largest element of the list using findMax defined in Q3. Use remove defined in Q2 and recursively call *)
  | [] -> []     (* selsort on the list with all instances of the previously-largest element removed. *)
  | [x] -> [x]
  | x::xs -> 
    if findMax xs > x then findMax xs::selsort(remove(findMax xs, l))
    else x::selsort(remove(x, l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with      (* Use memberof defined earlier to determine if the first element x of the first list is an element of the *)
  | ([],[]) -> []          (* second list. Add x to the common list if true, ignore if false, and recursively call common on the *)
  | ([],x::xs) -> []       (* whole y list and the shortened x list until x is exhausted. *)
  | (x::xs,[]) -> []
  | (x::xs,y::ys) ->
    if memberof(x,y::ys) then
      x::common(remove(x,xs),y::ys)
    else common(xs,y::ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  let rec helper l even odd =              (* Define a helper function which takes 3 lists as input. Take the first element of the first *) 
    match l with                           (* list l and prepend it to the first list even. Recursively call helper, swapping odd and even *)
    | [] -> (even,odd)                     (* until l is exhausted, then returning even and odd. even and odd are originally empty. *)
    | x::xs -> helper xs odd (x::even)
  helper l [] []

(* This is a former version of split. I left it here as I want to make it work. If you are grading this assignment, please ignore.*)
 (* match l with
  | [] -> []
  | x::[] -> ([x],[])::[]
  | x::y::[] -> ([x],[y])::[]
  | x::y::xs -> ([x],[y])::split(xs) {???} *)

let rec merge twolists = 
  match twolists with                   (* Given 2 ordered lists, compare the first element of each. Prepend the smallest of the two to the *)
  | ([],[]) -> []                       (* reconsituted list and recursively call merge on those lists, with the aforementioned smaller *)
  | (x, []) -> x                        (* element removed from its list, until one list is empty, then append the remaining list to the *)
  | ([], y) -> y                        (* return list and terminate merge. *)
  | (x::xs, y::ys) ->
    if x < y then x::merge(xs,y::ys)
    else y::merge(x::xs,ys)

let rec mergesort l =     (* Define two lists left and right by calling split on a provided list l. Recursively call mergesort on both of *)
  match l with            (* those lists, and finally merge them. This will order progressively larger lists until we finally have the *)
  | [] -> []              (* original list, but re-ordered. *)
  | [n] -> [n] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (left, right) = split (n::ns)
    merge(mergesort left, mergesort right)