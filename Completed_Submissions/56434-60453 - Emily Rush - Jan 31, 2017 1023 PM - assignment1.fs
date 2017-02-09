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

let rec sumlist l = 
  match l with
    | [] -> 0.0
    | x::xs -> x + (sumlist xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::(pairlists (xs, ys))

let w_mean weights data =  
  if ((weights = []) || (data = [])) then failwith "Please input non-empty lists."

  (* Compute denominator *)
  let denominator = sumlist weights

  (* Create new funciton to return *)
  let newFn  = fun reals ->
                  (* Compute numerator *)
                  let pairs = pairlists (reals, weights)
                  let product (x,y) = x * y
                  let prodlist = List.map product pairs
                  let numerator = sumlist prodlist
                  (* Compute weighted mean *)
                  numerator/denominator
  newFn data


(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
  | (a, []) -> false
  | (a, x::xs) ->
      if (a = x ) then true
      else memberof (a,xs)


let rec remove(item, lst) = 
  match lst with
  | [] -> []
  | x::xs ->
    if (x = item) then remove(item, xs)
    else x::(remove(item,xs))


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | y::ys ->
      if (y>m) then helper(ys,y)
      else helper(ys,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
  let rec helper4(oldlist, newlist) =
    match oldlist with
    | [] -> newlist
    | x::xs -> 
      let max = findMax oldlist
      helper4(remove(max,oldlist), newlist@[max])
  helper4(l,[])


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
  | ([],[]) -> []
  | (x::xs,[]) -> []
  | ([],y::ys) -> []
  | (x::xs,y::ys) ->
    if (x=y) then x::common(remove(x,xs),remove(y,ys))
    else if (remove(x,ys) <> ys) then x::common(remove(x,xs),remove(x,ys))
         else if (remove(y,xs) <> xs) then y::common(remove(y,xs),remove(y,ys))
              else common(remove(x,xs),remove(y,ys))


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with
    | [] -> ([],[])
    | x1::(x2::xs) -> 
        let lists = split xs
        match lists with
        | ([],[]) -> ([x1],[x2])
        | (y::ys,z::zs) -> (x1::(y::ys),x2::(z::zs)) 
        | ([],z::zs) -> ([x1],x2::(z::zs))
        | (y::ys,[]) -> (x1::(y::ys),[x2])
    | [x] -> ([x],[])

let rec merge twolists =
  match twolists with
  | ([],[]) -> []
  | (x::xs,[]) -> x::xs
  | ([],y::ys) -> y::ys
  | (x::xs,y::ys) -> 
    if (y>x) then x::(merge (xs,y::ys))
    else y::(merge (x::xs,ys))

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] 
  | n::ns -> 
    let (l1,l2) = split (n::ns)
    merge((mergesort l1), (mergesort l2))
