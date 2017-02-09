(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Anaelle Bitton, Id Number: 260575561 *) (* Edit this line. *)

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
    | x :: xs -> x + sumlist (xs) 


let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],y :: ys) -> failwith "Error -- lists are not of the same length"
    | (x :: xs, []) -> failwith "Error -- lists are not of the same length"
    | (x :: xs, y :: ys) -> (x,y) :: pairlists(xs,ys)


let w_mean weights data =  
    let l1 = sumlist weights
    let l2 = pairlists(weights,data)
    let l3 = l2 |> List.map ( fun (x,y) -> (x*y) ) 
    sumlist(l3)/(l1)

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = // failwith "Not implemented"
    match pair with 
    | (x,[])-> false
    | (n,x :: xs) -> if x = n then true else memberof (n,xs) 

let rec remove(item, lst) = 
    match lst with 
    | [] -> []
    | x :: xs -> if x = item then remove(item,xs) else  x::remove(item,xs) 


(* Question 3. *)  (* Do not edit this line. *)
let findMax l =
    let rec helper(l,m)= 
        match l, m with
        | [], m -> m
        | (l :: ls), m -> if l < m then helper(ls,m) else helper(ls,l) 

    match l with
    | [] -> failwith "Error -- empty list"
    | (x :: xs) -> helper(xs,x)
  

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | [] -> []
    | l ->  let max = findMax l 
            let list = remove(max,l)
            max :: selsort list

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
   match twolists with 
    | ([],[]) -> []
    | ([],y :: ys) -> failwith "Error -- lists are not of the same length"
    | (x :: xs, []) -> failwith "Error -- lists are not of the same length"
    | x :: xs , y :: ys -> if y = x then x :: common (xs,ys) else common(ys, x :: xs) 
(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or some other sort defeats the whole purpose.*)
let rec split l = 
    match l with
    | [] -> ([], [])
    | [x] -> ([x], [])
    | x :: y :: rest -> 
        let (a,b) = split rest in (x :: a, y :: b)

let rec merge twolists = 
    match twolists with
    | ([],[]) -> []    
    | (x :: xs,y :: ys) -> if x < y then x :: y :: merge(xs,ys) else y :: x :: merge(xs,ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n :: []) -> n :: [] (* Without this you will go into an infinite loop. *)
  | n :: ns -> 
    let l1,l2 = split(l) 
    merge( mergesort(l1) , mergesort(l2))

