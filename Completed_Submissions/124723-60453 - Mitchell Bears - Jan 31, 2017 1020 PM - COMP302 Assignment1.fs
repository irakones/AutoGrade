(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Mitchell Bears, Id Number: 260675975 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGNMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)

(* module hw1_sol.  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)

let rec sumlist l= 
    match l with
      | [] -> 0.0
      | x::xs -> (x:float) + sumlist xs

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)


let w_mean weights data =
  sumlist(pairlists(weights,data) |> List.map(fun (x,y) -> x*y))/sumlist weights
     
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
  match pair with
    |(x,[]) -> false
    |(x, y::ys) -> if x = y then true else memberof(x,ys)


let rec remove(item, lst) =
  match lst with
    |[] -> lst
    |y::ys -> if item = y then remove(item,ys) else y::remove(item,ys)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match (l,m) with
    |([],m) -> m
    |(x::xs,m) -> if x > m then helper(xs,x) else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
  match l with
  | [] -> []
  | x::xs -> if memberof(x,xs) then selsort(xs) else findMax l :: selsort(remove(findMax l, l))


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
  |([],[]) -> []
  |([],x::xs) -> []
  |(x::xs, []) -> []
  |(x::xs, y::ys) -> if x = y then x::common(selsort(xs),selsort(ys)) elif memberof(x,ys) then x::common(selsort(xs), remove(x,selsort(y::ys))) elif memberof(y,xs) then y::common(selsort(ys), remove(y,selsort(xs))) else common(selsort(xs),selsort(ys)) 



(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  match l with
  | [] -> ([],[])
  | [x] -> ([x], [])
  | x::y::xs -> let (first,second) = split xs in (x::first,y::second)
  
                

let rec merge twolists = 
  match twolists with
  | ([],[]) -> []
  | ([],xs) -> xs
  | (xs,[]) -> xs
  | (x::xs, y::ys) -> if x <= y then x::merge(xs, y::ys) else y::merge(ys, x::xs)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (lst1, lst2) = (split l) in merge(mergesort lst1, mergesort lst2)
 