(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Yahya Azami, Id Number: 260535376 *) (* Edit this line. *)

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
        | hd::tail -> hd + sumlist(tail)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y)::pairlists(xs, ys)

let second (_, s) = s

let rec extract_weighted_data lst = 
    match lst with
      | [] -> []
      | head::tail -> second(head)::extract_weighted_data(tail)

let w_mean weights data =
    let a = pairlists((weights:float list), data) in
    let b = a |> List.map (fun (x, y) -> (x, x * y)) in
    let c = extract_weighted_data(b) in
    sumlist(c)


(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
    | (_, []) -> false
    | (i, hd::tl) -> if (i = hd) then true else memberof(i, tl)

let rec remove(item, lst) = 
  match (item, lst) with
    | (_, []) -> []
    | (i, (hd::tl)) -> if (i = hd) then remove(i, tl) else hd::(remove(i, tl))


(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
  let rec helper(l,m) = 
    match l with
      |[] -> m
      | hd::tail -> if m < hd then helper(tail, hd)
                    else helper(tail, m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)

  
let rec selsort l = 
    match l with
      | [] -> []
      | lst -> let max = findMax(l) in
                   let rest = remove(max, l) in
                   max::selsort(rest)


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
    | ([], lst) -> []
    | (lst, []) -> []
    | (hd::tail, lst) -> if memberof(hd, lst) then
                           let cleanlst = remove(hd, lst) in  
                           hd::common(tail, cleanlst)
                         else common(tail, lst)



(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  match l with 
    | [] -> ([], [])
    | [hd] -> ([], [hd])
    | hd1::hd2::tail -> let (lst1, lst2) = split(tail) in
                        (hd1::lst1, hd2::lst2)

let rec merge twolists = 
  match twolists with
    | (lst1, []) -> lst1
    | ([], lst2) -> lst2
    | (hd1::tail1, hd2::tail2) -> if hd1 < hd2 then hd1::merge(tail1, hd2::tail2)
                                  else hd2::merge(hd1::tail1, tail2)


let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (lst1,lst2) = split(l) in
             let slist1 = mergesort(lst1) in
             let slist2 = mergesort(lst2) in
             merge(slist1, slist2)


