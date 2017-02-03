(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Robert Belfer, Id Number: 260634641 *) (* Edit this line. *)

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

let rec sumlist l:float= 
    match l with
        | [] -> 0.0
        | x::xs -> x + sumlist(xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)

let rec dot pairlists =
    match pairlists with
    | ([]) -> 0.0
    | (x,y)::xys -> x*y+dot(xys)


let w_mean weights data =  dot(pairlists(weights, data))/sumlist(weights)
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (item, []) -> false
    | (item, x::xs) -> if (item=x) then true else memberof(item,xs)

let rec remove(item, lst) = 
    match (item, lst) with
    | (item, []) -> []
    | (item, x::xs) -> if (item=x) then remove(item,xs) else x::remove(item,xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    | [x] -> if(x>m) then x else m
    | x::xs -> if(x>m) then helper(xs,x) else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | [x] -> x
  | x::xs -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | [] -> []
    | x::xs ->
        let m = findMax(l)
        m :: selsort(remove(m,l))


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with
    |([], ys) -> []
    |(x::xs,ys) -> if(memberof(x,ys)) then x::common(remove(x,xs), ys) else common(remove(x,xs), ys)


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    let rec helper(plist, slow, fast, mode)=
        match plist, slow, fast, mode with
        |[], [], [], _ -> ([], [])
        |plist, slow, [], _ -> plist, slow
        |plist, s::slows, f::fasts, false -> helper(s::plist, slows, fasts, true)
        |plist, s::slows, f::fasts, true -> helper(plist, s::slows, fasts, false)
    helper([], l,l,false)

let rec merge twolists = 
    match twolists with
    | ([], l2) -> l2
    | (l1, []) -> l1
    | (x::xs, y::ys) -> if(x<y) then x::merge(xs, y::ys) else y::merge(x::xs, ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (l1, l2) = split(l)
             let s1 = mergesort(l1)
             let s2 = mergesort(l2)
             merge(s1,s2)   

