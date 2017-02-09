(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Judith Schmitz, Id Number: 260205513 *) (* Edit this line. *)

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
(*compute the sum of a list of floats*)
  match l with 
  | [] -> 0.0 (*empty list*)
  | (x::xs) -> x + sumlist xs

let rec pairlists twolists =
(*take two lists of floats the same length and produces a list of matched pairs*)
  match twolists with
  | ([],[]) -> []
  | ([],x::xs) -> failwith "Error -- lists are not of the same length"
  | (x::xs, []) -> failwith "Error -- lists are not of the same length"
  | (x::xs, y::ys) -> (x , y) :: pairlists (xs, ys) 

let w_mean weights data =  
(*computes the weighted mean: sum real*weights / sum weights*)
  let rec calculation list1 =
(*auxiliary function to add paired values*)
    match list1 with
    | [] -> []
    | (x, y) :: bs -> (x*y) :: calculation (bs)  (*adding the paired values in the list, b are the rest of the pairs*)
  (sumlist (calculation (pairlists (weights, data))) / sumlist(weights)) 
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
(*tests whether an element is a member of a given list*)
  match pair with
  | item, [] -> false (*base case empty list*)
  | (item, (x::xs)) -> if x = item then true (*if first number if equal to item, return*)
                       else memberof(item, xs) (*otherwise recurse on smaller list*)

let rec remove(item, lst) = 
(*takes an element and a list and removes all copies of the element from list or return same list*)
  match lst with
  | [] -> []
  | (x::xs) -> if x = item then remove(item, xs) (*if first number is equal to item, remove it*)
               else x::remove(item, xs) (*otherwise include first number in new list and recurse*) 

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
(*finds the largest value of a list of elements of a comparison type*)
  let rec helper(l,m) = 
    match (l,m) with
    | [], m -> m 
    | (x::xs), m -> helper (xs, if (x > m) then x else m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
(*largest element chosen and put infront, rest of list is recursively sorted*)
(*we may re-use findMax from Q3*)
  match l with 
  | [] -> []
  | (x::xs) -> if xs = [] then [x]
               elif x >= findMax(xs) then x::(selsort(remove(x, xs)))
               else findMax(l)::selsort(remove(findMax(l), l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
(*takes a pair of lists and forms a new list containing a unique copy of each element that occurs in both*)
  match twolists with
  | [],[] -> []
  | items, [] -> []
  | [], items -> [] (*if a list is empty, or both empty, return empty list*)
  | ((one), y::ys) -> if memberof(y, one) then y::(common(ys, remove(y, one)))
                      else common(one, ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  match l with
  | [] -> [], []
  | [oneitem] -> ([oneitem],[])
  | x::y::xs -> let (first, second) = split(xs) (*split xs and put x and y together*)
                (x::first, y::second)

let rec merge twolists = 
(*merges sorted lists*)
  match twolists with
  | ([], []) -> [] 
  | (a, []) -> a
  | ([], b) -> b
  | (x::xs, y::ys) -> if x < y then x::merge(xs, y::ys) 
                      else y::merge(x::xs, ys)

let rec mergesort l = 
(*implements the mergesort algorithm to sort lists*)
  match l with
  | [] -> [] (*base case*)
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | (n::ns) -> let (one, two) = split(l)
               let sortedone = mergesort(one)
               let sortedtwo = mergesort(two)
               merge(sortedone, sortedtwo)