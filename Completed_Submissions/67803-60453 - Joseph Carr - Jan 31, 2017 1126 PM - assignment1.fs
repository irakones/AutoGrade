(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Joseph Carr, Id Number: 260520178 *) (* Edit this line. *)

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
   | x :: xs -> x + sumlist xs
 
let rec pairlists twolists =
   match twolists with
      | ([],[]) -> []
      | ([],x::xs) -> failwith "Error -- lists are not of the same length"
      | (x::xs, []) -> failwith "Error -- lists are not of the same length"
      | (x::xs, y::ys) -> (x,y)::pairlists (xs,ys)
 
let w_mean weights data =
   let ans data =
      let pairedup = pairlists (weights,data)
      let multPair (x,y) = x*y
      let ans = pairedup |> List.map multPair
      (sumlist ans)/(sumlist weights)
   ans data
 
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
   let (element,list) = pair
   match list with
      | [] -> false
      | x::xs ->
         if (element=x) then true
         else memberof (element,xs)


let rec remove(item,lst) =
   match lst with
      | [] -> []
      | x::xs ->
         if (item=x) then remove(item,xs)
         else x::remove(item,xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l =
   let rec helper(l,m) =
      match l with
      | [] -> m
      | (x::xs) ->
         if (x>m) then helper(xs,x)
         else helper(xs,m)
   match l with
      | [] -> failwith "Error -- empty list"
      | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)

//Note this function replies upon findMax and remove from the 2 previous questions.
let rec selsort l =
   if (l=[]) then []
   else
      let max = findMax l
      max::selsort(remove(max,l))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
   let rec helper(l,m) =
      match l with
         | [] -> false
         | (x::xs) ->
            if (x=m) then true
            else helper(xs,m)
 
   let (lst1,lst2) = twolists
   match lst1 with
      | [] -> []
      | (x::xs) ->
         if (helper(lst2,x)) then x::common(xs,lst2)
         else common(lst2,xs)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
   match l with
      | [] -> ([],[])
      | (x::[]) -> ([x],[])
      | (x1::x2::xs) ->
         let (ans1,ans2)=split xs
         (x1::ans1,x2::ans2)
 
let rec merge twolists =
   match twolists with
      | ([],[]) -> []
      | ([],x::xs) -> x::xs
      | (x::xs, []) -> x::xs
      | (x::xs, y::ys) ->
         if (x<y) then x::merge (xs,y::ys)
         else y::merge (x::xs,ys)
 
let rec mergesort l =
   match l with
      | [] -> []
      | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
      | _ ->
         let (x1,x2) = split l
         let (ans1,ans2) = (mergesort x1, mergesort x2)
         merge(ans1,ans2)
