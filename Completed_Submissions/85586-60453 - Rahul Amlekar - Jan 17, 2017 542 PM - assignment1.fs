(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Rahul Amlekar, Id Number: 260553904 *) (* Edit this line. *)

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
    | x :: xs -> sumlist(xs) + x

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Lists of unequal length"
    | (x::xs, []) -> failwith "Lists of unequal length"
    | (x::xs, y::ys) ->  (x,y) :: pairlists (xs, ys)

let w_mean weights data =
    let listOfPairs = pairlists(weights, data)
    let rec multiplyHelper listOfPairs =
        match listOfPairs with
        | [] -> []
        | (x1, x2) :: xs ->  x1 * x2 * 1.0 :: multiplyHelper(xs)
    sumlist(multiplyHelper(listOfPairs)) / sumlist weights


(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (var,[]) -> false
    | (var,x :: xs) -> 
        if (x = var) then true 
        else memberof (var, xs)

let rec remove(item, lst) = 
    match lst with
    | [] -> []
    | x :: xs -> 
        if (x = item) then remove(item, xs)
        else x :: remove(item, xs)


(* Question 3. *)  (* Do not edit this line. *)
let findMax l =
    let rec helper(l,m) =
        match l with
        | [] -> m
        | (x::xs) -> 
            if (x > m) then helper(xs,x)
            else helper(xs,m)
    match l with
    | [] -> failwith "Empty list"
    | x::xs -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)
let rec selsort l = 
  match l with
  | [] -> []
  | _ -> let m = (findMax l)
         m::(selsort(remove(m,l)))


(* Question 5. *)  (* Do not edit this line. *)

(*The commented function below has better time efficiency, but it is commented as the type is 
'a list * 'a list -> 'a list(requires comparison) rather than
'a list * 'a list -> 'a list(requires equality)
 
let rec common twolists = 
    let list1 = selsort (fst twolists)
    let list2 = selsort (snd twolists)
    let helper (list1, list2)=
      match twolists with
      |([],[]) -> []
      |([], y::ys) -> []
      |(x::xs, []) -> []
      |(x::xs, y::ys) -> 
          if (x = y) then x :: (common (remove(x,xs), remove(y,ys)))
          elif (x > y) then common(x::xs, remove(y,ys))
          else common(remove(x,xs), y::ys)
      helper (list1, list2)
*)

let rec common twolists = 
    match twolists with
    | ([], _) -> []
    | (_, []) -> []
    | (x::xs, y::ys) -> if (memberof(x, y::ys)) then x :: common (remove(x,xs), remove(y, y::ys))
                        else common(y::ys, remove(x,xs))


(* Question 6. *)   (* Do not edit this line. *)
let rec split l = 
   match l with
   | [] -> ([], [])
   | [x] -> ([x], [])
   | (x1::x2::xs) -> let firstPart, secondPart = split(xs)
                     (x1 :: firstPart, x2 :: secondPart)
let rec merge twolists = 
  match twolists with
  | ([],[]) -> []
  | (x::xs, []) -> x::xs
  | ([], y::ys) -> y::ys
  | (x::xs, y::ys) -> 
    if x < y then x :: merge(xs, y::ys)
    else y :: merge(x::xs, ys)
let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | var -> let firstHalf, secondHalf = split(var)
           merge (mergesort(firstHalf), mergesort(secondHalf))