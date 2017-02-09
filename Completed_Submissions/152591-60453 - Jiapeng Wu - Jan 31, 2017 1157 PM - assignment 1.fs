(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Jiapeng Wu, Id Number: 260727743 *) (* Edit this line. *)

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
    |[] -> 0.0
    |x::xs -> x + sumlist xs


let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x , y) :: pairlists(xs, ys)


let w_mean weights data = 
    let rec sumlist l = 
        match l with
        |[] -> 0.0
        |x::xs -> x + sumlist xs
    let rec pairlists twolists =
        match twolists with
        | ([],[]) -> []
        | ([],x::xs) -> failwith "Error -- lists are not of the same length"
        | (x::xs, []) -> failwith "Error -- lists are not of the same length"
        | (x::xs, y::ys) -> x * y :: pairlists(xs, ys)
    sumlist (pairlists (weights, data)) / sumlist(weights)
    


(* Question 2. *)  (* Do not edit this line. *)




let rec memberof pair = 
    match pair with
    | (n,[]) -> false
    | n,x::xs -> (n = x)||(memberof(n, xs))



let rec remove(item, lst) = 
    let rec helper(l1,l2) = 
        match l1 with
        | [] -> l2
        | x::xs -> helper(xs, x::l2)
    let rec helper1(l1,l2) = 
        match l1 with
        | [] -> l2
        | x::xs -> if ((x = item) = false) then helper1(xs, x::l2)
                   else helper1(xs, l2)
    let rec memberof pair = 
        match pair with
        | (n,[]) -> false
        | (n,x::xs) -> if (n = x) then true else memberof(n, xs)
    if (memberof(item, lst) = false) then lst 
    else
        helper(helper1(lst,[]),[])


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = 
     match l with
     | [] -> m
     | x::xs -> if (m > x) then helper(xs,m)
                else
                    helper(xs,x)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)
// let testList = [1;10;3;6;1;8;3;11;6;5;9]
// findMax testList


(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    let findMax l = 
        let rec helper(l,m) = 
            match l with
            | [] -> m
            | x::xs -> if (m > x) then helper(xs,m)
                        else
                            helper(xs,x)
        match l with
        | [] -> failwith "Error -- empty list"
        | (x::xs) -> helper(xs,x)

    let rec remove(item, lst) = 
        let rec helper(l1,l2) = 
            match l1 with
            | [] -> l2
            | x::xs -> helper(xs, x::l2)
        let rec helper1(l1,l2) = 
            match l1 with
            | [] -> l2
            | x::xs -> if ((x = item) = false) then helper1(xs, x::l2)
                       else helper1(xs, l2)
        let rec memberof pair = 
            match pair with
            | (n,[]) -> false
            | (n,x::xs) -> if (n = x) then true else memberof(n, xs)
        if (memberof(item, lst) = false) then lst 
        else
            helper(helper1(lst,[]),[])

    match l with
    | [] -> []
    | x::xs -> findMax(x::xs)::selsort (remove(findMax (x::xs), (x::xs)))
               

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    let rec remove(item, lst) = 
        let rec helper(l1,l2) = 
            match l1 with
            | [] -> l2
            | x::xs -> helper(xs, x::l2)
        let rec helper1(l1,l2) = 
            match l1 with
            | [] -> l2
            | x::xs -> if ((x = item) = false) then helper1(xs, x::l2)
                       else helper1(xs, l2)
        let rec memberof pair = 
            match pair with
            | (n,[]) -> false
            | (n,x::xs) -> if (n = x) then true else memberof(n, xs)
        if (memberof(item, lst) = false) then lst 
        else
            helper(helper1(lst,[]),[])

    let rec memberof pair = 
        match pair with
        | (n,[]) -> false
        | (n,x::xs) -> if (n = x) then true else memberof(n, xs)
    match twolists with
    | ([],[]) -> []
    | (x::xs, []) -> []
    | ([],x::xs) -> []
    | (x::xs,y::ys) -> if (memberof (x,y::ys)) then x::common (xs,remove(y,y::ys))
                       else common (xs,y::ys)


(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l = 
    let removefirst l = 
        match l with
        |[] -> []
        |x::xs -> xs
    let rec getonelist l =
        match l with
        |[] -> []
        |x::xs -> x::getonelist(removefirst(xs))
    
    match l with
    |[] -> ([],[])
    |x::xs -> (getonelist(x::xs),getonelist(xs))


let rec merge twolists =
    match twolists with
    |([],[]) -> []
    |(x::xs,[]) -> x::xs
    |([],y::ys) -> y::ys
    |(x::xs,y::ys) -> if(x > y) then y::merge(x::xs,ys)
                      else x::merge(xs,y::ys)


let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> let (a,b) = split(n::ns)
             merge(mergesort(a),mergesort(b))


            
