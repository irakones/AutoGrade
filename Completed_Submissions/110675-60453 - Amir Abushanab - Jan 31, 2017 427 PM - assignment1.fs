(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Amir Abushanab, Id Number: 260624419 *) (* Edit this line. *)

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
    |x::xs -> x + sumlist(xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> [x,y]@pairlists(xs,ys)

let w_mean weights data = 
  let rec helper pairedlist =
    match pairedlist with
    |[] -> []
    |(x,y)::k -> [x*y]@helper(k)
  sumlist(helper(pairlists(weights,data)))/sumlist(weights)



  
(* Question 2. *)  (* Do not edit this line. *)
let rec memberof (y,l) =
    match l with
    | [] -> false
    | x::xs -> if y = x then true
               else memberof (y,xs) 

let rec remove (y,l) =
    match l with
    | [] -> l
    | x::xs -> if x=y then remove(y,xs) 
               else x::remove(y,xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l with
    | [] -> m
    |(y::ys) -> if m < y then helper(ys,y)
                else helper (ys,m)
    
  match l with
  | [] -> failwith "Error -- empty list"
  |(x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
  let findMax l = 
    let rec helper(l,m) =
      match l with
      | [] -> m
      |(y::ys) -> if m < y then helper(ys,y)
                  else helper (ys,m)
      
    match l with
    | [] -> failwith "Error -- empty list"
    |(x::xs) -> helper(xs,x)

  let removeduplicates duplicatelist = //Not sure if we are allowed to do this, buuuuut it was shown in class and you never said we weren't allowed, I'll take the risk
    duplicatelist|>Set.ofList|> Set.toList

  match l with
  | [] -> l
  | (x::xs) -> removeduplicates(findMax(l)::x::selsort(xs))


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  
  let rec memberof (y,l) =
      match l with
      | [] -> false
      | x::xs -> if y = x then true
                 else memberof (y,xs)

  let removeduplicates duplicatelist = //Not sure if we are allowed to do this, buuuuut it was shown in class and you never said we weren't allowed, I'll take the risk
    duplicatelist|>Set.ofList|> Set.toList

  let rec helper (l1,l2) =
    match l1 with
    |[] -> []
    |k::ks -> if memberof(k,l2) then k::helper(ks,l2)
              else helper(ks,l2)

  match twolists with
  | ([],[]) -> []
  | (x::xs, y::ys) -> helper(removeduplicates(x::xs),removeduplicates(y::ys))

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let rec split l =
  match l with
  | [] -> ([],[])
  | [x] -> ([x],[])
  | (x::y::xs) -> let (firsthalf,secondhalf) = split xs
                  (x::firsthalf,y::secondhalf)
               
let rec merge twolists =
  match twolists with
    | ([],[]) -> []
    | ([],l) -> l 
    | (l,[]) -> l
    | (x::xs, y::ys) -> if x<y then x::merge (xs,y::ys)
                        else y::merge (x::xs,ys)
let rec mergesort l = 
  match l with
  | [] -> []
  | [n] -> [n] (* Without this you will go into an infinite loop. *)
  | l -> let (l1, l2) = split l
         merge (mergesort l1,mergesort l2)
  



