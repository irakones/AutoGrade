(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Chuqing Zhang, Id Number: 260620385 *) (* Edit this line. *)

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
    |[] -> 0.0
    |x::xs -> x + (sumlist xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> []
    | (x::xs, []) -> []
    | (x::xs, y::ys) -> (x,y) :: (pairlists (xs,ys))


let w_mean weights data = 
    match data with
    |[] -> 0.0
    |x::xs -> 
            let l = pairlists (weights, data) in
                let r1 = l |> List.map (fun (s,p) -> s*p) in
                     (sumlist r1)/(sumlist weights)

(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    |(x,[]) -> false
    |(x, y::ys) -> if x = y then true
                    else memberof (x, ys)

let rec remove(item, lst) = 
    match lst with
    |[] -> []
    |x::xs -> if item = x then remove(item, xs)
                else x::remove(item, xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  
  let rec helper(l,m) = 
    match l with
    |[] -> m
    |y::ys -> if y > m then helper(ys, y) else helper(ys, m) 
  
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    let rec helper(n,lst) =
        match lst with
        |[] -> [n]
        |x::xs -> if n>x then n::lst else x::(helper(n,xs))

    match l with 
    |[] -> []
    |x::xs -> let ys=remove(x,xs) in
              helper(x,(selsort ys))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with
    |([],[]) -> []
    |(x::xs,[]) -> []
    |([],x::xs) -> []
    |(x::xs, y::ys) -> let r1=remove(x,xs) in
                         if memberof(x,y::ys) then x::(common (r1,y::ys))
                         else common (r1,y::ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with
    |[] -> ([],[])
    |[x] -> ([x],[])
    |x1::x2::xs -> let (l1,l2) = split xs in
                    (x1::l1,x2::l2)

let rec merge twolists = 
    match twolists with
    |([],[]) -> []
    |(x::xs,[]) -> x::xs
    |([],x::xs) -> x::xs
    |(x::xs, y::ys) -> if x < y then x::(merge (xs,y::ys))
                        else y::(merge (x::xs, ys))

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (l1,l2) = split l in
                merge((mergesort l1),(mergesort l2))