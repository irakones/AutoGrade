(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: YiQi Christina Lin, Id Number: 260561048 *) (* Edit this line. *)

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
let rec sumlist l:float =
    let rec helper lst v =
        match lst with
        |[] -> v
        |x::xs -> helper xs (v+x)
    helper l 0.0


let rec pairlists twolists =
  match twolists with
  | ([],[]) -> []
  | ([],x::xs) -> failwith "Error -- lists are not of the same length"
  | (x::xs, []) -> failwith "Error -- lists are not of the same length"
  | (x::xs, y::ys) -> (x,y):: pairlists (xs,ys)


let w_mean weights data =
    let lst = pairlists (weights,data)
    let rec product l =
        match l with
        | [] -> [] 
        | x::xs -> let (s,p) = x
                   (s*p)::(product xs)
    (sumlist (product lst)) / (sumlist weights)

  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (x,[]) -> false
    | (x, y::ys) -> if (x=y) then true
                    else memberof (x,ys)

let rec remove(item, lst) =
    match lst with
    | [] -> lst
    | x::xs -> if (x=item) then remove(item,xs)
               else x::remove(item,xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =
    match l with
    | [] -> m
    | x::xs -> if (x<m) then helper(xs,m)
               else helper(xs,x)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)



(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | [] -> []
    | x::xs -> let max = findMax l
               max::(selsort (remove (max,l)))   


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
    match twolists with
    | ([],[]) -> []
    | (l1, []) -> []
    | ([],l2) -> [] 
    | (x::xs , y::ys) -> if memberof (x,(y::ys)) then 
                                                    let x1 = remove (x,xs)
                                                    let y1 = remove (x,(y::ys))
                                                    x::common(x1, y1)
                         else common (xs, (y::ys))


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)


let rec split l = 
    match l with 
    | [] -> ([],[])
    | [x] -> ([x],[])
    | x1::x2::xs -> let (l1,l2) = split xs
                    (x1::l1,x2::l2)


let rec merge twolists =
    match twolists with 
    | ([],[]) -> []
    | (l1, []) -> l1
    | ([], l2) -> l2
    | (x::xs , y::ys) -> if (x < y) then x::(merge (xs, y::ys))
                         else y::(merge (x::xs,ys))

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (l,r) = split (n::ns)
             merge (mergesort l, mergesort r)
