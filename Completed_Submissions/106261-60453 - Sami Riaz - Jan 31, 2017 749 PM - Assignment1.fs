(*Assignment 1*)(* Do not edit this line. *)
(* Student name: Sami Riaz, ID Number: 260630430 *) 

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGNMENT even if the problem is only with one
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
        | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)


let w_mean weights data =  
    let l1 = pairlists(weights,data) // pair up x with corresponding w
    let l2 = l1 |> List.map(fun (x,y) -> x*y) // multiply x with corresponding w
    let numerator = sumlist l2 // numerator = sum of all x*w
    let denominator = sumlist weights // denominator = sum of weights
    numerator/denominator // (sum of all x*w)/(sum of weights) = weighted mean 



(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    match pair with
    | (a,[]) -> false
    | (a,x::xs) -> if (a = x) then true else memberof(a,xs)


let rec remove(item, lst) = 
 match lst with
 | [] -> []
 | x::xs -> if item = x then remove(x,xs) 
              else x::remove(item,xs)

   


(* Question 3. *)  (* Do not edit this line. *)
let findMax l = 
    let rec helper(l,m) = 
        match l with
        | [] -> m   
        | (x::xs) -> if m < x then helper(xs,x)
                     else m
    match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper (xs,x)


(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | [] -> []
    | l ->  findMax l :: selsort (remove (findMax l ,l))  

(* Question 5. *)  (* Do not edit this line. *)
let rec common twolists = 
    let rec helper(twolists,m) = 
        match twolists with
        | ([], []) -> m
        | (l1, []) -> m  
        | ([], l1) -> m
        | (x::xs, y::ys) -> if memberof (x, y::ys) then helper((xs, y::ys), m@[x])
                            else helper ((xs, y::ys), m)
    helper(twolists,[]) 


   (* Question 6 *)   (* Do not edit this line. *)
  
   (* Mergesort requires that you use recursion.  Using isort or
    some other sort defeats the whole purpose.*)

let rec split l = 
    let rec helper (l, l1, l2) = 
        match l with
        | [] -> (l1, l2)
        | [x] -> (l1@[x], l2) // if only 1 item in list, it is split into 2 lists with one being empty
        | x::y::zs -> helper(zs, l1@[x], l2@[y]) //
    helper(l,[],[])

let rec merge twolists = 
    let rec helper (l1,l2) = 
        match l1 with
        |([],[]) -> l2
        |(x::xs,[]) -> helper((xs,[]), l2@[x])
        |([],y::ys) -> helper(([],ys),l2@[y])
        |([x],[y]) -> // when one item in each list
                    if(x<y) then (l2@[x]@[y]) // if x<y append x to lst first
                    else (l2@[y]@[x]) // else append y to lst first
        |(x::xs,y::ys) ->
                    if(x<y) then helper((xs,y::ys),l2@[x])
                    else helper((x::xs,ys),l2@[y]) 
    helper(twolists,[])


let rec mergesort l = 
    match l with
    | [] -> []
    | ([n]) -> [n] (* Without this you will go into an infinite loop. *)
    | n::ns ->  let (l1,l2) = split l
                merge ((mergesort l1),(mergesort l2))

