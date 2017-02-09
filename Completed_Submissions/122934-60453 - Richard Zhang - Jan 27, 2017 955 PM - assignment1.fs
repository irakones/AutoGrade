(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Richard Zhang, Id Number: 260664443 *)

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

let rec sumlist l= 
    match l with 
    | [] -> float(0)
    | x::y -> x + sumlist y

let rec pairlists twolists =
    match twolists with
    | ([], []) -> []
    | ([], x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x, y)::(pairlists (xs, ys))

let w_mean weights data =
    let rec helper twolists acc: float = 
        match twolists with
        | [] -> acc
        | x::y -> helper y (acc + (fst x * snd x))
    let div = sumlist weights
    let twolists = pairlists (weights, data)
    (helper twolists 0.0) / div
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof (x, listx) =
    match listx with
    | [] -> false
    | y::listy -> 
        if (x = y) then true
        else memberof (x, listy)

let rec remove (x, listx) = 
    match listx with
    | [] -> listx
    | y::listy -> 
        if x = y then remove (x, listy)
        else y::(remove (x, listy))

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
    let rec helper (l,m) = 
        match l with
        | [] -> m
        | y::listy ->
            if (y > m) then helper (listy, y)
            else helper (listy, m)
    match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
    let findMax_helper1 l = 
        let rec helper (l,m) = 
            match l with
            | [] -> m
            | y::listy ->
                if (y > m) then helper (listy, y)
                else helper (listy, m)
        match l with
        | [] -> failwith "Error -- empty list"
        | (x::xs) -> helper(xs,x)

    let rec remove_helper1 (x, listx) = 
        match listx with
        | [] -> listx
        | y::listy -> 
            if x = y then remove_helper1 (x, listy)
            else y::(remove_helper1 (x, listy))
    
    match l with 
    | [] -> []
    | [x] -> [x]
    | x::xs -> 
        let m = findMax_helper1 l
        if (m = x) then x::(selsort (remove_helper1 (m, l)))
        else m::(selsort (remove_helper1 (m, l)))

(* Question 5. *)  (* Do not edit this line. *)

let rec common (listx, listy) = 
    let rec remove_helper2 (x, listx) = 
        match listx with
        | [] -> listx
        | y::listy -> 
            if x = y then remove_helper2 (x, listy)
            else y::(remove_helper2 (x, listy))
    let rec memberof_helper1 (x, listx) =
        match listx with
        | [] -> false
        | y::listy -> 
            if (x = y) then true
            else memberof_helper1 (x, listy)
    match listx with
    | [] -> []
    | z::listz -> 
        if memberof_helper1 (z, listy) then z::common(remove_helper2(z, listz), listy)
        else common(listz, listy)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with 
    | [] -> ([], [])
    | [x] -> ([x], [])
    | x::y::xs ->
        let (l1, l2) = split xs
        (x::l1, y::l2)

let rec merge (listx, listy) = 
    match listx with
    | [] -> listy
    | s::lists ->
        match listy with
        | [] -> listx
        | u::listu ->
            if s < u then s::(merge (lists, listy))
            else u::(merge (listx, listu))

let rec mergesort l = 
    match l with
    | [] -> []
    | (n::[]) -> n::[]
    | n::ns -> 
        merge (mergesort (fst(split l)),  mergesort (snd(split l)))



