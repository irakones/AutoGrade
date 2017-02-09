
(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Dale Mowbray, Id Number: 260639463*) (* Edit this line. *)

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

let rec sumlist (l : float list) =
    match l with
    |[] -> failwith "Error -- list is empty"
    |[x] -> x
    |x::xs -> x + sumlist xs

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists (xs,ys)


let w_mean (weights : float list) (data : float list) =  
    let wx =
        let pairs = pairlists (weights,data)
        let wxs = pairs |> List.map (fun (w,d) -> w*d)
        sumlist wxs
    let w =
        sumlist weights
    wx/w
 
 
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (m, []) -> false
    | (m,x::xs) -> if m = x then true
                   else memberof (m,xs) 


let rec remove(item, lst) = 
    match lst with
    | [] -> []
    | x::xs -> if x = item then remove(item, xs)
               else x::remove(item,xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
    let rec helper(l,m) = 
        match l with
        | [] -> m
        | x::xs -> if x > m then helper(xs,x)
                   else helper(xs,m)
    match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l  = 
    match l with
    | [] -> []
    | [x] -> [x]
    | x::xs -> if findMax xs > x then findMax xs::selsort(remove(findMax xs, x::xs))
               else x::selsort(remove(x,xs))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> []
    | (x::xs, []) -> []
    | (x::xs, y::ys) -> if memberof(x, ys) || x = y then x::common(remove(x,xs), remove(x,y::ys))
                        else common(remove(x,xs), y::ys)


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with
    |[] -> [],[]
    |x::[] -> [x],[]
    |x0::x1::[] -> ([x0],[x1])
    |x0::x1::xs -> let (a,b) = split xs
                   (x0::a,x1::b)


let rec merge twolists =
    match twolists with 
    | ([],[]) -> []
    | ([],y::ys) -> y::ys
    | (x::xs, []) -> x::xs
    | (x::xs, y::ys) -> if x < y then x::merge(xs,y::ys)
                        else y::merge(x::xs,ys)
    

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (left, right) = split l
             merge(mergesort(left),mergesort(right))
             



