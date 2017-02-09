
(* Student name: Mark Slavsky, Id Number: 260675515 *) (* Edit this line. *)

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
    | [] -> 0.
    | x::xs -> x + (sumlist xs)     


let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: (pairlists (xs,ys))

let w_mean weights data =
  let denom = sumlist weights
  let pairs = pairlists (weights, data)
  (sumlist (List.map (fun (x,y) -> x * y) pairs))/denom

  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
    |(v, []) -> false
    |(v, x::xs) -> if v=x then true else memberof (v,xs)


let rec remove(item, lst) = 
  match lst with
    |[] -> []
    |x::xs -> if item=x then remove(item, xs) else x::remove (item,xs)



(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
      |[] -> m
      |x::xs -> if x > m then helper(xs, x) else helper(xs, m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  match l with
    |[] -> []
    | _ -> let m = findMax l 
           m :: selsort (remove(m,l))
            


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
    |([],[])->[]
    |(x::xs, []) | ([], x::xs) -> []
    |(x::xs, y::ys) -> if memberof (x, y::ys) then x::common (xs, y::ys) else common (xs,y::ys)


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  let rec splitHelp lst lst1 lst2 =
    match lst with
      |[] -> (lst1,lst2)
      |[x] -> (lst1@[x], lst2) 
      |(x::y::xys) ->  splitHelp xys (lst1@[x]) (lst2@[y])
  splitHelp l [] []

let rec merge twolists = 
    let rec aux lsts result =
      match lsts with
        | ([], []) -> result
        | ([], x::xs) | (x::xs, []) -> aux ([],xs) (result@[x])
        | (x::xs,x2::xs2) ->if x < x2 then aux (xs, x2::xs2) (result@[x]) else aux (x::xs,xs2) (result@[x2])
    aux twolists []



let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (X,Y) = split l 
             merge (mergesort X, mergesort Y)



