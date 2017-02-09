(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Chris-Jorjio Issa, Id Number: 260638851 *) (* Edit this line. *)

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
    | [] -> 0.0
    | (x::xs) -> x + sumlist xs

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) ->
      let result = pairlists(xs,ys)
      (x,y)::result

let w_mean weights data =
  let r1 = pairlists(weights,data)|> List.map (fun (x,y) -> (x * y):float) (* We use *)
  sumlist(r1) / sumlist(weights)

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
    |(_,[]) -> false
    |(value, x::xs) -> 
    if value = x then true
    else
      memberof(value,xs)
let rec remove(item, lst) = 
  match (item,lst) with 
    |(_,[]) -> []
    |(item, x::xs) -> 
    if (item = x) then remove(item, xs)
    else
      x::remove(item,xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l with 
      | [] -> m
      | (x::xs) -> if x > m then helper(xs,x) else helper(xs,m)
      
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
    match l with
      | [] -> l
      | (x::xs) ->
        let temp = findMax(l)
        temp::selsort (remove(temp, x::xs))


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  match twolists with
    | [],[] -> []
    | [],x::xs -> []
    | x::xs,[] -> []
    | (x::xs, y::ys) -> if memberof(x,y::ys) then x::common(remove(x,x::xs), y::ys) else common(remove(x,xs), remove(y,y::ys)) (* The usage of remove here is to get rid of duplicates. *)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let split l = 
  let rec helper (l,l1,l2) = 
    match l with
      | [] -> l1,l2
      | (x::xs) -> helper(xs, x::l2, l1)
  helper(l,[],[])  

let rec merge twolists =
  match twolists with
    | [],[] -> []
    | [],x::xs -> x::xs
    | x::xs,[] -> x::xs
    | (x::xs, y::ys) -> if x <= y then x::merge(xs,y::ys) else y::merge(ys,x::xs)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (l1,l2) = split l 
    merge(mergesort(l1),mergesort(l2))
    
    



