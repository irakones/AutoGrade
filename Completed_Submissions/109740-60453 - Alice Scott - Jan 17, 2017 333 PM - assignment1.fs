(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Alice Scott, Id Number: 260631443 *) 

(* module hw1_sol.  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)
let rec sumlist l= 
    match l with
    | [] -> 0.0 //base case
    | x :: xs -> x + sumlist xs //recursive call

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: pairlists (xs, ys)

let w_mean weights data = 
  let l = pairlists(weights, data) |> List.map (fun (x,y) -> x * y)
  match l with
  | [] -> 0.0
  | x -> (sumlist l) / (sumlist weights)
              
                
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (x, []) -> false
    | (x, y::ys) -> if (x = y) then true else (memberof (x,ys))


let rec remove(item, lst) =
    match lst with
    | [] -> []
    | x::xs -> if (x = item) then (remove(item, xs))
               else x::(remove(item, xs))


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
      match l with
      | [] -> m
      | x::xs -> if (x > m) then helper(xs, x) else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
    match l with
    | [] -> []
    | x::xs -> (findMax l)::selsort(remove(findMax l, l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    let commlist = []
    match twolists with
    | ([], []) -> []
    | (x::xs, []) -> []
    | ([], y::ys) -> []
    | (x::xs, y::ys) -> if (memberof(x,y::ys)) then x::common(xs, ys)
                        else common(xs,y::ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
    match l with
    | [] -> ([], []) //base case 1
    | [x] -> ([x], []) //base case 2
    | x::y::remaining -> let (l1, l2) = split remaining
                         (x::l1, y::l2)

let rec merge twolists = 
    match twolists with
    | (xs, []) -> xs //base case 1
    | ([], ys) -> ys //base case 2
    | (x::xs, y::ys) -> if (x < y) then x::y::merge(xs,ys)
                        elif (x = y) then x::y::merge(xs,ys)
                        else y::x::merge(xs,ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (l1,l2) = split l
             merge(mergesort l1, mergesort l2)


