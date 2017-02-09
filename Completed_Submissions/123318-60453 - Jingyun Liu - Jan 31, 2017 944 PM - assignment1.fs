(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Jingyun LIU, Id Number: 260657058 *) (* Edit this line. *)

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

let rec sumlist (l:float list) = match l with 
    |x::xs -> (float)x + sumlist xs
    | [] -> (float)0


let rec pairlists pair =
  match pair with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y):: (pairlists (xs,ys) )

let rec product (l1:float list, l2:float list) = 
    match (l1,l2)with
    |([],[]) -> []
    | (x::xs, y::ys) -> (float) (x*y)::(product (xs,ys))
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"

let w_mean weights data= 
    let weight_total = sumlist(weights)
    let calc datas = 
        let prod = product (weights, datas) in 
        sumlist(prod)/weight_total
    calc data
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    |(x, y::ys) -> if (x=y) then true else (memberof (x,ys))
    |(x, []) -> false


let rec remove pair = 
    match pair with
    | (item, []) -> []
    | (item, x::xs) -> if (item = x) then xs else (x::remove (item,xs))


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
    let rec helper(l,m) = match (l, m) with
                            | (x::xs, m) -> if (x>m) then (helper (xs,x)) else (helper (xs,m))
                            | ([], x) -> x

    match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | [] -> []
    | l2 -> (findMax l2) :: selsort(remove(findMax l2, l2))
    
(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with
    | (x::xs, l2) -> 
                    let lst = common(xs, l2)
                    if (not (memberof (x, lst)) && memberof (x, l2)) then x::lst
                    else lst
    | ([], l2) -> [];

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l= 
    match l with
    | [] -> ([],[])
    | x::[] -> ([x],[])
    | x::y::xs -> let (A,B) = split xs
                  (x::A, y::B)

let rec merge twolists = match twolists with
    |(l,[]) -> l 
    |([],l) -> l 
    |(x::xs, y::ys) -> if (x>y) then (y::x::(merge (xs,ys))) else (x::y::(merge (xs,ys)))


let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | (n::ns )-> let (l1,l2) = split l
               let A1 = mergesort l1
               let A2 = mergesort l2
               merge (A1, A2)


