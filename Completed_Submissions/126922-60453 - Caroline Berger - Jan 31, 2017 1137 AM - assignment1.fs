(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Caroline Berger, Id Number: 260671067 *) (* Edit this line. *)

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
        | [] -> 0.0  (* the sum of an empty list is 0 *)
        | x::xs -> x + sumlist xs
                
let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y) :: (pairlists (xs, ys))

(* mult takes a paired list as input and returns a list of the pairs multiplied by
one another note:mult is recursive *)
let rec mult prodlist =
    match prodlist with
    | [] -> []
    | (x, y)::full -> (x*y) ::  (mult(full))

let w_mean weights data = 
    ((sumlist(mult(pairlists(data, weights)))) / (sumlist(weights)))

(* end quesiton 1 *)

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with 
    | _,[] -> false (* nothing is a member of the empty list, this is our base case *)
    | y, x::xs -> if y = x then true else memberof(y,xs)

(* if an empty list, return the empty list *)
let rec remove(item, lst) = 
    match item, lst with
    | _, [] -> lst
    | _, x::xs -> if item = x then remove(item, xs) else x::remove(item, xs)

(* end quesiton 2 *)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l,m with 
    | [],cmax -> cmax
    | y::ys, cmax -> if y>cmax then helper(ys, y) else helper(ys,cmax)
     
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* end question 3 *)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | [] -> []
    | l -> findMax l::selsort(remove(findMax l,l))

(* end questions 4 *)


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with 
    | _, [] -> []
    | [], _ -> []
    | (r::rs),fs -> if memberof(r, fs) then r::common(rs, fs) else common(rs,fs)
  
(* end question 5 *)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with
    | []-> ([], [])
    | [x] -> ([x], []) (* if there is only one element in the list *)
    | x :: y :: xs ->
        let (l1, l2) = split xs in (x :: l1, y :: l2)
         
let rec merge twolists = 
    match twolists with
    | (l1, []) -> l1
    | ([], l2) -> l2
    | x::xs, y::ys -> if x<y then x :: merge(xs,y::ys ) else y :: merge (x::xs, ys)


let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (s1, s2) = split l
             merge (mergesort s1, mergesort s2)
