(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Xin Miao Lin, Id Number: 260632603 *) (* Edit this line. *)

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
    | x::xs -> x + sumlist xs

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> 
        let newtwolsts = (xs, ys)
        (x, y)::pairlists newtwolsts

let rec helper prodlst = 
    match prodlst with
    | ([],[]) -> []
    | ([],x::xs) -> []
    | (x::xs, []) -> []
    | (x::xs, y::ys) -> 
        let newtwolsts = (xs, ys)
        x*y::helper newtwolsts


let w_mean weights data = sumlist (helper(weights, data)) / sumlist weights
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
    match pair with
    | (x,[]) -> false
    | (x,y::ys) ->
        if (x=y) then true
        else memberof(x, ys) 

let rec remove(item, lst) = 
    if memberof(item, lst) then
        match lst with
        | [] -> []
        | x::xs ->
            if x=item then remove(item, xs)
            else x::remove(item, xs)
    else
        lst

(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | x::xs ->
      if x>m then helper(xs, x)
      else helper(xs, m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)

let rec selsort l = 
  match l with
  | [] -> []
  | [x] -> [x] 
  | x::xs ->
    let maxnum = findMax xs
    if x=maxnum then
      let lst = remove(maxnum, xs)
      x::selsort lst
    elif x>maxnum then
      x::selsort xs
    else 
      let lst = x::remove(maxnum, xs)
      maxnum::selsort lst

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
  | ([],[]) -> []
  | ([],x::xs) -> []
  | (x::xs,[]) -> []
  | (x::xs,y::ys) ->
    if memberof (x, y::ys) then
      if memberof (x, xs) then
        let lst = remove(x, xs)
        x::common(lst, y::ys)
      else x::common(xs, y::ys)
    else common(xs, y::ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec append pair1 pair2 = 
  match pair1 with
  | ([],[]) -> pair2
  | (x::xs,[]) ->
    match pair2 with
    | ([],[]) -> pair1
    | (y::ys,[]) -> append (x::y::xs,[]) (ys,[])
    | ([],y::ys) -> (x::xs,y::ys)
    | (y::ys,z::zs) -> append (x::y::xs,z::zs) (ys,[])
  | ([],w::ws) ->
    match pair2 with  
    | ([],[]) -> pair1
    | (y::ys,[]) -> (y::ys,w::ws)
    | ([],y::ys) -> append ([],y::w::ws) ([],ys)
    | (y::ys,z::zs) -> append (y::ys,z::w::ws) ([],zs)
  | (x::xs,w::ws) ->
    match pair2 with
    | ([],[]) -> pair1
    | (y::ys,[]) -> append (x::y::xs,w::ws) (ys,[])
    | ([],y::ys) -> append (x::xs,y::ws) ([],ys)
    | (y::ys,z::zs) -> append (x::y::xs,w::z::ws) (ys,zs)
let rec split l =
  match l with 
  | [] -> ([],[])
  | [x] -> ([x],[])
  | x::xs ->
    match xs with
    | [] -> ([x],[])
    | [y] -> ([x],[y])
    | y::ys ->
      append ([x],[y]) (split ys)
      

let rec merge twolists = 
  match twolists with
  | ([],[]) -> []
  | (x::xs,[]) -> 
    match xs with
    | [] -> [x]
    | y::ys -> 
      if x<y then
        x::(merge (y::ys,[]))
      else 
        y::(merge (x::ys,[]))
  | ([],x::xs) -> 
    match xs with
    | [] -> [x]
    | y::ys ->
      if x<y then
        x::(merge ([],y::ys))
      else 
        y::(merge ([],x::ys))
  | (x::xs,y::ys) ->
    if x>y then 
      y::(merge (x::xs,ys))
    else 
      x::(merge (xs,y::ys))

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let lst = n::ns
    match (split lst) with
    | ([],[]) -> []
    | (x::xs,[]) -> x::mergesort(xs)
    | ([],x::xs) -> x::mergesort(xs)
    | (x::xs,y::ys) -> 
      let lst1 = mergesort (x::xs)
      let lst2 = mergesort (y::ys)
      merge (lst1, lst2)