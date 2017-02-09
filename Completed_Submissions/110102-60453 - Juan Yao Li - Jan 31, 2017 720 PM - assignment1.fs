(* Assignment 1 *) (* Do not edit this line. *)
(* Student name:Juan Yao Li (Justin), Id Number: 260640228 *) (* Edit this line. *)

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
        | x::xs -> x + sumlist xs


let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::(pairlists (xs,ys))



let w_mean weights data=
    let rec product l:float list=
        match l with
        |[] -> []
        |(x,y)::xs->x*y::product xs
    (sumlist (product (pairlists (weights,data)) ) )/ (sumlist weights)

  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (_,[]) -> false
    | (x, y::ys) -> if x=y then true else memberof (x,ys)


let rec remove(item, lst) = 
    match (item,lst) with
    | (_,[]) -> []
    | (x, y::ys) -> if x <> y then y:: remove(x,ys) else remove (x,ys)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with 
    |[] -> m
    |(x::xs) -> if x>m then helper(xs, x) else helper (xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l = 
    match l with
    | [] -> [] 
    | [x] -> [x]
    | x::xs -> let max = findMax xs
               if x > max  then     
                    x:: selsort (remove(x,xs))  
               else 
                    max::selsort (x:: remove(max, xs ))


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with
    | ([],[]) -> []
    | (l, []) -> []
    | ([], l) -> []
    | (x::xs, y::ys) -> if x=y then 
                             x::common(remove(x,remove(y,xs)), remove(y,remove(x,ys)))
                        elif memberof(x,ys) && memberof(y,xs) then 
                            x::y::common(remove(x,remove(y,xs)), remove(y,remove(x,ys)))
                        elif memberof(x,ys) then
                            x::common(remove(x,xs),remove(y,remove(x,ys)))
                        elif memberof(y,xs) then
                            y::common(remove(x,remove(y,xs)), remove(y,ys))
                        else 
                            common(remove(y,remove(x,xs)),remove(x,remove(y,ys)))

                        
(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with
    |[] -> ([],[])
    |[x] -> ([x],[])
    |x::y::rest-> let (a,b) = split(rest)
                  (x::a, y::b)


let rec merge twolists = 
  match twolists with
  |([],[])->[]
  |(x,[])->x
  |([],x)->x
  |(x::xs,y::ys) -> if x>y then 
                        y::merge(x::xs,ys)
                    else
                        x::merge(xs,y::ys)


let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (x,y) = split (n::ns)
             merge  ((mergesort x), (mergesort y))

