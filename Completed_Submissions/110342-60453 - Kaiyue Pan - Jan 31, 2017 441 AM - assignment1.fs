(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Kaiyue Pan, Id Number: 260627397 *) (* Edit this line. *)

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
  match l with
    | [] -> 0.0
    | x :: xs -> 
          let s = sumlist xs
          x+s

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::(pairlists(xs,ys))

let w_mean weights data =
    let product (x,y) = x*y 
    sumlist(List.map product (pairlists(data,weights)))/sumlist(weights)

  
(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair =
  match pair with
  | (x,[]) -> false
  | (x,y::ys) -> if (x = y) then true
                  else (memberof(x,ys))

let rec remove (item, lst) = 
  let rec helper (l1,l2)=
    match l1 with
    | [] -> []
    | x::xs -> if (x = item) then helper (xs,l2)
                 else (x::helper(xs,l2))
  helper(lst,[])


(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
  let rec helper(l,m) =
    match l with
    | [] -> m
    | x::xs -> if (x>m) then helper(xs,x)
                else helper(xs,m)
                
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l =
  
  match l with
  | [] -> []
  | _ -> 
    let max = findMax l
    max::(selsort(remove(max,l)))
            

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
  let rec helper ((l1,l2),l3)=
    match l1 with
    | [] -> l3
    | x::xs -> if (memberof(x,l2)) then (helper((xs,l2),x::l3))
                  else (helper ((xs,l2),l3))
  helper(twolists,[])


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  let rec helper lst (lst1,lst2) = 
    match lst with
    | [] -> (lst1,lst2)
    | [x] -> (x::lst1,lst2)
    | x1::x2::rest -> helper rest (x1::lst1,x2::lst2)
  
  helper l ([],[])


let rec merge twolists =
  match twolists with
  | ([],[]) -> []
  | ([],x::xs) -> x::xs
  | (x::xs, []) -> x::xs
  | (x::xs, y::ys) -> if (x<y) then (x::(merge(xs,y::ys)))
                        else (y::(merge(x::xs,ys)))


let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | l -> 
    let (le,ri)=split l
    merge(mergesort(le),mergesort(ri))


