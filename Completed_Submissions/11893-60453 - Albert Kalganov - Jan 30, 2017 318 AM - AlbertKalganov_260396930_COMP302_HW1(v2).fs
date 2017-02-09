(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Albert Kalganov, Id Number: 260396930 *) (* Edit this line. *)

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

let rec sumlist l =     // (l:float list)
    match l with
    |[] -> 0.0
    |x::xs -> x+sumlist(xs)

let rec pairlists twolists = 
    match twolists with 
    | ([],[]) -> [] 
    | ([],x::xs) -> failwith "Error -- lists are not of the same length" 
    | (x::xs, []) -> failwith "Error -- lists are not of the same length" 
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys) 

let rec multpairs (pairlist: float list*float list) = 
    match pairlist with 
    | ([],[]) -> [] 
    | ([],x::xs) -> failwith "Error -- lists are not of the same length" 
    | (x::xs, []) -> failwith "Error -- lists are not of the same length" 
    | (x::xs, y::ys) -> (x*y)::multpairs(xs,ys)

let w_mean weights data = 
    let wd=(weights,data)
    let pairedlist=multpairs wd
    let sum1=sumlist pairedlist
    let sum2=sumlist weights
    if sum2=0.0 then failwith "Error: devision by zero not defined!"   // devision by zero conditional
    else sum1/sum2
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =     
    let el1,el2=pair
    match el2 with
    |[]->false
    |x::xs-> if el1=x then true
             else memberof(el1,xs)


let rec remove(item, lst) =     
    match lst with
    |[]->[]
    |x::xs->if x=item then remove(item,xs)
            else x::remove(item,xs) 


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
    let rec helper(l,m) =     
       match l with
       |[]->m
       |x::xs->if x>m then helper(xs,x)
               else helper(xs,m)
    match l with 
    | [] -> failwith "Error -- empty list"         
    | (x::xs) -> helper(xs,x) 
    

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =        
    match l with
    |[]->[]
    |[x]->[x]
    |x::xs-> let max=findMax xs
             if (x>max) then x::(selsort xs)                        
             elif (max>x) then max::(selsort(x::remove(max,xs)));   
             else selsort xs                         

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =     
    let l1,l2=twolists
    match twolists with
    |([],[])->[]
    |([],_)->[]
    |(_,[])->[]
    |(x::xs,y::ys)-> if (memberof(x,y::ys)) then x::common(remove(x,xs),remove(y,ys))
                     elif (memberof(y,x::xs)) then y::common(remove(x,xs),remove(y,ys))
                     else common(remove(x,xs),remove(y,ys)) 

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
    match l with
    |[]->([],[])
    |[x]->([x],[])
    |x::y::xys->let (xl,yl)=split xys
                (x::xl,y::yl) 

let rec merge twolists =     
    match twolists with
    |([],[])->[]
    |([],x::xs)->x::xs 
    |(x::xs,[])->x::xs    
    |(x::xs,y::ys)-> if (x<y) then x::merge(xs,y::ys)
                     else y::merge(x::xs,ys)

let rec mergesort l =       
    match l with 
    | [] -> [] 
    | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *) 
    | n::ns -> let l1,l2=split (n::ns)
               merge(mergesort l1, mergesort l2) 



