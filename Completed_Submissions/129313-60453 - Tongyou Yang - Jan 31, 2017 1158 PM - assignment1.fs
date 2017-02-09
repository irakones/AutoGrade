(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Alfred E. Neumann, Id Number: 17294104 *) (* Edit this line. *)

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

let rec sumlist (list: float list) = 
  let rec helper(x,y) =
    match x with
    | [] -> y
    | x:: xs -> helper(xs,y+x)
  helper(list, 0.0)

let rec pairlists ( list1, list2 ) =
    let rec helper(x,y,z) =
        match (x,y) with
        | ([],[]) -> z
        | (x :: xs, y :: ys) -> helper(xs, ys, (x,y)::z)
        | ([], y :: ys) -> failwith "Error -- lists are not of the same length"
        | (x :: xs, []) -> failwith "Error -- lists are not of the same length"
    helper(list1, list2, [])

let rec w_mean (weights: float list) (data: float list) =
    let rec helper(x, y) = 
        match x with 
        | [] -> y
        | o :: os -> 
            let a, b = o
            helper(os, y+a*b)
    helper ((pairlists (weights, data)), 0.0) / sumlist weights
  

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof (int, list) = 
    match list with
    | [] -> false
    | x :: xs -> if (x = int ) then true
                 else 
                    memberof(int, xs)


let rec remove (int, list) =
    match list with
    | [] -> []
    | x :: xs -> if (x = int) then remove(int, xs)
                 else
                    x :: remove(int, xs)


(* Question 3. *)  (* Do not edit this line. *)

let rec findMax list =
    let rec helper(l,m) =
        match l with 
        | [] -> m
        | x :: xs -> if ( x > m ) then helper(xs, x)
                     else 
                        helper(xs, m)
    match list with 
    | [] -> failwith "Error -- empty list"
    | x :: xs -> helper(xs, x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
    match l with 
    | [] -> []
    | x :: xs -> findMax l :: selsort (remove (findMax l, l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common (list1, list2) =
    let rec helper (l1, l2, result) =
        match (l1, l2) with
        | ([],[]) -> result
        | (x :: xs, []) -> result
        | ([], y :: ys) -> result
        | (x :: xs, y :: ys) -> if (memberof (x, l2)) then helper (l2, xs, x :: result)
                                else helper (xs, l2, result)
    helper((selsort list1),(selsort list2), [])

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
    let rec helper (list, l1, l2) =
        match list with
        | [] -> (l1, l2)
        | x :: xs -> helper(xs, l2, x :: l1)
    helper(l, [], [])

let rec merge (l1, l2) =
    let rec helper1 (list1, list2, result) =
        match (list1, list2) with
        | ([],[]) -> result
        | (x :: xs, []) -> helper1(xs, list2, x :: result)
        | ([], y :: ys) -> helper1(list1, ys, y:: result)
        | (x :: xs, y :: ys) -> if (x < y) then helper1(xs, list2, x :: result)
                                else helper1(list1, ys, y :: result)
        
    let rec helper2 (p,q) =
        match p with 
        | [] -> q
        | x :: xs -> helper2 (xs, x::q)
    helper2( helper1(l1,l2, []), [])

let rec mergesort l = 
    match l with
    | [] -> []
    | (n::[]) -> n::[]
    | n::ns -> let (a,b) = split l
               merge( (mergesort a), (mergesort b) )

