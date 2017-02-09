(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Lu Ya Ding, Id Number: 260627893 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)


(*module hw1_sol  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)

let rec sumlist l = 
    match l with
    | [] -> 0.0
    | x::xs -> sumlist(xs) + x

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)        

let w_mean weights data = 
    let sumOfWeights = sumlist weights
    let plist = pairlists (weights,(data : float list))
    let prodT (a:float, b:float) = a * b
    let rec prodSum listz =
      match listz with
        | [] -> 0.0
        | x::xs -> prodT(x) + prodSum (xs)
    prodSum(plist) / sumOfWeights


  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
     | (_,[]) -> false
     | (y,x::xs) -> if (x = y) then true else memberof (y,xs)



let rec remove(item, lst) = 
    match lst with
     | [] -> []
     | x::xs -> if (item <> x) then x::remove(item,xs) else remove(item,xs)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | x::xs -> if (x>m) 
               then let m = x
                    helper(xs,m) 
               else helper(xs,m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | [] -> []
    | x::xs -> 
      let max = findMax(l)
      max::selsort(remove(max,l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> []
    | (x::xs,[]) -> []
    | (x::xs,y::ys) -> if (x=y) then x::common(xs,ys)
                       elif memberof(x,ys) then x::common(xs,ys)
                       elif memberof(y,xs) then y::common(xs,ys)
                       else common(xs,ys)


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    match l with
    | [] -> ([],[])
    | [x] -> ([x],[])
    | x::y::zs -> let (m,n) = split(zs)
                  (x::m,y::n)

let rec merge twolists = 
    match twolists with
    | ([], ys) -> ys
    | (xs, []) -> xs
    | (x::xs, y::ys) -> if x < y 
                        then x :: merge (xs, y::ys)
                        else y :: merge (x::xs, ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (x,y) = split l
             merge (mergesort x,mergesort y)



