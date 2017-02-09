(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Vincent Tam, Id Number: 260378527 *) 

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
  //| x::xs -> List.sum l
  | (x::xs) -> x + sumlist xs //This will add the first element with each element in the list in a recursive manner

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::(pairlists(xs,ys))  //Recursively pairing elements of the two lists

//w_mean [1.0;2.0] [2.0;4.0];;
let w_mean weights data =  
  match weights with
  | [] -> 0.0
  | (x::xs) -> sumlist(pairlists(weights,data) |> List.map (fun (x,y) -> x * y))/sumlist(weights)
    (*Call pairlists to create a list of matched pairs. Then use List.map to apply 
      the function x * y to multiply the elements in every pair. Divide by the sum of weights *)
    //let product = pairlists(weights,data) |> List.map (fun (x,y) -> x * y)
    //mean = sumlist(product)/sumlist(weights)
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
  match pair with
  | (n,[]) -> false   // Empty list, hence false
  | (n,(x::xs)) -> if (n=x) then true else memberof(n,xs) // recursively test if n is equal to the first element of the updated list


let rec remove(item, lst) = 
  match lst with
  | [] -> []    //empty list, then just return empty list
  | (x::xs) -> if (item=x) then remove(item,xs) else x::(remove(item,xs)) 
    (*if the item is not equal to the first element of the list, then append to output list*)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
    match l with
    | [] -> m
    | (y::ys) -> 
      if (y > m) then helper(ys,y)     //if we find a bigger a value, replace it and call helper function recursively
      else 
        helper(ys,m)    

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)   

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
  match l with
  | [] -> []
  | (x::xs) -> 
    //if memberof(x,xs) then x::selsort(remove(x,xs))
    //else x::selsort(xs)
    let maxvalue = findMax(l)     //find the max value
    maxvalue::selsort(remove(maxvalue,l))   
    (*create new list starting with the first maxvalue, remove maxvalue found from the current list, apply selsort recursively*)

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
  match twolists with
  | ([],[]) -> []
  | (x::xs, []) -> []
  | ([], x::xs) -> []
  | (x::xs, y::ys) -> 
      (*test if x is element of the 2nd list, if yes, generate new list starting with x 
      and then remove x from 2nd list. Apply common function recursively with new lists*)
      if memberof(x,y::ys) then x::(common(xs, remove(x,y::ys))) 
      else common(xs,y::ys)  

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
  match l with
  | [] -> ([],[])
  | (x::[]) -> (x::[],[])
  | (x::y::l) -> 
    let (L1,L2) = split l in 
      (x::L1,y::L2)  (*Divide the list into two*)

let rec merge twolists = 
  match twolists with
  | (LA,[]) -> LA     //if one list is empty, return the other list
  | ([],LB) -> LB
  | (x::xs,y::ys) -> 
      if (x > y) then y::merge(x::xs,ys)    //smaller values first
      else x::merge(xs,y::ys)
 
let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop.*)
  | n::ns -> 
    let (L,R) = split l in
      let Lsub = mergesort(L)   //merge and sort each sub problem
      let Rsub = mergesort(R) in
        merge(Lsub,Rsub)   //divide and conquer, solve each subproblem