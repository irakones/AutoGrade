(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Alice Tilles, Id Number: 260579661 *) (* Edit this line. *)

(* module hw1_sol.  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)

let rec sumlist list =
    match list with
    | [] -> 0.0
    | x :: xs ->  x + (sumlist xs)

let rec pairlists twolists =
    match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: pairlists (xs,ys)

let rec w_mean weights data = 
    let pairs = pairlists (weights,data)
    let denominator = sumlist weights 

    let rec multiplypairs pairinput =
        match pairinput with
        | [] -> 0.0
        | x :: xs -> match x with | (a,b) -> a * b + (multiplypairs xs)
    
    (multiplypairs pairs) / denominator
    

(* Question 2. *)  (* Do not edit this line. *)
let rec memberof pair =
    let searchfor, list = pair
    match list with
    | [] -> false
    | x :: xs -> if searchfor = x then true else memberof(searchfor,xs)


let rec remove (item,lst) =
    match lst with
    | [] -> lst
    | x :: xs -> if x=item then xs else x :: remove (item,xs)

(* Question 3. *)  (* Do not edit this line. *)
let findMax list = 
  let rec helper max list =
    match list with
    | [] -> max
    | x :: xs -> if x>max then helper x xs else helper max xs

  match list with
  | [] -> failwith "Error -- empty list"
  | x :: xs -> helper x list


(* Question 4. *)  (* Do not edit this line. *)
let rec selsort list =
    let max = findMax list

    let updated = remove (max,list)
    if memberof (max,updated) then updated else
        match updated with
        | [] -> [max]
        | _ -> max :: selsort updated

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    let first,second = twolists

    let rec helper (list1,list2,incommon) =
        match list1 with
        | [] -> incommon
        | x :: xs -> if memberof (x,list2) && not (memberof (x, incommon)) then helper (xs,list2, x::incommon) else helper (xs,list2,incommon)

    helper (first,second,[])




(* Question 6. *)   (* Do not edit this line. *)

(* 'split' can not handle splitting an empty list. *)
let rec split list =

    let rec splithelper (input,list1,list2) =
        match input with
        | [] -> (list1,list2)
        | x :: xs -> splithelper (xs,x::list2, list1)

    splithelper (list, [], [])


let rec merge (twolists) = 
    let list1,list2 = twolists

    match list1 with
    | [] -> list2
    | list1item :: list1remainder -> 
    match list2 with
    | [] -> list1
    | list2item :: list2remainder -> if list1item < list2item then list1item :: merge (list1remainder, list2) else list2item :: merge (list1, list2remainder) 

let rec mergesort (l) = 


    let p = split l
    let a,b = p
    match l with
    | [] -> []
    | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
    | _ -> merge (mergesort a, mergesort b)
    



