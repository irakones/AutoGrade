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

let rec sumlist l: float = //failwith "Not implemented"
    match l with
    | [] -> 0.0
    | x:: xs -> let (sum) = sumlist(xs)
                (x + sum)


let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) ->  failwith "Error -- lists are not of the same length"
    | (x::xs, []) ->  failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) ->(x,y) :: (pairlists(xs, ys))//failwith "Not implemented"

let w_mean weights data =  //failwith "Not implemented"
  
  let holder = pairlists(weights,data)
  let pairmult = holder |> List.map(fun(x,y) -> (x*y))
  let weightedMean = (sumlist pairmult) / (sumlist weights)
  weightedMean


                     
  

  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =  ///failwith "Not implemented"
    match pair with
    | (t,y::ys) -> (t = y) || (memberof (t,ys))
    | (t,[]) -> false



let rec remove(item, lst) = //failwith "Not implemented"
    match lst with
    | [] -> []
    | x::xs -> if x = item then remove(x,xs)
                else 
                    x :: remove(item, xs)



(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = /// failwith "Not implemented"
      match l with
      | [] -> m
      | x::xs -> if x > m then helper(xs,x)
                 else helper(xs,m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line.  *)
  
let rec selsort l = //failwith "Not implemented"
    match l with
    | [] -> []
    | x::xs -> let max = findMax(l)
               let newlist = remove(max,l)
               max :: selsort(newlist)
               


(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = ///failwith "Not implemented"
    match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> []
    | (x::xs,[]) -> []
    | (x::xs,y::ys) ->  let currentlist = remove(x,xs)
                        if memberof(x,y::ys) then  x::common(currentlist,y::ys)
                        else common(currentlist,y::ys)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = //failwith "Not implemented"
    match l with
    | [] -> ([],[])
    | [x] -> ([x],[])
    | x1::x2 :: xs -> let (firstlist,secondlist) = split(xs)
                      (x1 :: firstlist, x2 :: secondlist)

let rec merge twolists = //failwith "Not implemented"
    match twolists with
    | ([],x) -> x
    | (y,[]) -> y
    | ([],[]) -> ([]) 
    | (x::xs,y::ys) -> if x=y then x::y::merge(xs,ys)
                       elif x<y then x::merge(xs,y::ys)
                       else  y::merge(x::xs,ys)
                    

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (list1,list2) = split(n::ns)//failwith "Not implemented"
             merge(mergesort list1,mergesort list2)

