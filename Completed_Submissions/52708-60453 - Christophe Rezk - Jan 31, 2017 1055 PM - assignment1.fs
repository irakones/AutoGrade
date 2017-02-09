(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Christophe Rezk, Id Number: 260501293 *) (* Edit this line. *)

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
    | [] -> 0.00
    | (x::xs) -> x + sumlist(xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)

let rec multiply l =
  match l with
    | [] -> []
    | (a,b)::xs -> a*b::multiply (xs)

let w_mean weights data =  
  sumlist(multiply(pairlists(weights,data)))/(sumlist(weights))

  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with 
    | (m,[]) -> false
    | (m,(x::xs)) -> if (x=m) then true
                     else memberof(m,xs)
    
let rec remove(item, lst) = 
  match (item, lst) with
    | (m,[]) -> []
    | (m,(x::xs)) -> if (x=m) then remove(m,xs)
                     else x::(remove(m,xs))


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match (l,m) with
      | ([],m) -> m
      | (x::xs,m) -> helper(xs,max m x) 
  match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)

let rec removedup l =
  match l with
    | [] -> []
    | x :: xs -> remove(x, x::xs) @ removedup(xs)
  
let rec selsort l = 
  match l with
    | [] -> []
    | x :: xs -> let a = findMax (x::xs) in a :: selsort(remove (a,x::xs))


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with 
    | (l,[]) -> []
    | ([],l) -> []
    | ((x::xs),l) -> if memberof(x,l) then x::(common(xs,remove(x,l)))
                     else common(xs,l)
                     
 
(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  match l with
    | []        -> ([],[])
    | x::[]     -> (x::[],[])   
    | (x::y::l) -> let (oddls,evenls) = split(l) in (x::oddls,y::evenls)   

let rec merge twolists = 
  match twolists with   
    | ([],a) -> a
    | (b,[]) -> b 
    | (x::xs, y::ys) -> if (x<y) then x::merge(xs,y::ys)     
                        else (y::merge(x::xs,ys))
let rec mergesort l = 
  match l with
    | [] -> []
    | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
    | n::ns -> let (c,d) = split(l) in 
                let e = mergesort(c)      
                let f = mergesort(d) in     
                  merge(e,f)

