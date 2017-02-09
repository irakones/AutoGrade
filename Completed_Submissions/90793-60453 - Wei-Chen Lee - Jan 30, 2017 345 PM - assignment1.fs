(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Wei-Chen Lee (Vivian), Id Number: 260556629 *) (* Edit this line. *)

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
        | (x::xs, y::ys) -> (x,y) :: pairlists (xs, ys)
        
    let w_mean weights data = 
      let list = pairlists (weights, data) 
      let rec mult list2 = 
        match list2 with
          | [] -> []
          | x::xs -> let (u,v) = x  
                     (u*v)::(mult xs)
      (sumlist (mult list)) / (sumlist weights)
                        
      
    (* Question 2. *)  (* Do not edit this line. *)
    let rec memberof pair = 
      match pair with
        | (x, y::ys) -> if (x = y) then true
                            else memberof (x, ys)
        | (x, []) -> false 
    
    let rec remove(item, lst) = 
      match lst with
        | [] -> []
        | x :: xs -> if (item = x) then remove(item, xs)
                     else x::remove(item, xs)
                           

    (* Question 3. *)  (* Do not edit this line. *)
    let findMax l = 
      let rec helper(l,m) = 
        match l with
          | [] -> m
          | x :: xs -> if (m < x) then helper(xs, x)
                        else helper (xs, m)
      match l with
      | [] -> failwith "Error -- empty list"
      | (x::xs) -> helper(xs,x)



    (* Question 4. *)  (* Do not edit this line. *)  
    let rec selsort l =
      match l with
        | [] -> []
        | x :: xs -> let max = findMax l 
                     if (x = max) then let list = remove(x,l)
                                       x::selsort list
                     else selsort (xs@[x])

    (* Question 5. *)  (* Do not edit this line. *)
    let rec common twolists = 
      let rec helper twolists acc  = 
        match twolists with 
          | ([], []) -> acc
          | (x::xs, []) -> acc
          | ([], y::ys) -> acc
          | (x::xs, y::ys) -> if memberof (x,y::ys) then let list3= remove(x, x::xs)
                                                         let list4= remove(y, y::ys)
                                                         helper (list3, list4) (x::acc)
                              else let list5= remove(x, x::xs)
                                   helper (list5, y::ys)  acc 
      helper twolists []
        
        
         
                    
    (* Question 6. *)   (* Do not edit this line. *)

    (* Mergesort requires that you use recursion.  Using isort or
    some other sort defeats the whole purpose.*) 
    let rec split l = 
      let rec count list = 
        match list with
        | [] -> 0
        | x::xs -> 1 + count(xs)
      let rec helper list1 list2 l = 
        match l with
         | [] -> list1, list2
         | x::xs -> let length = count l
                    if (length % 2 = 0) then helper (list1@[x]) list2 xs
                    else helper list1 (list2@[x]) xs
      helper [ ] [ ] l

    let rec merge twolists = 
      match twolists with 
         | ([], ys) -> ys
         | (xs, []) -> xs
         | ((x::xs as xAll), (y::ys as yAll)) -> if (x<y) then x :: merge(xs,yAll)
                                                 else y :: merge(xAll, ys)
        

    let rec mergesort l = 
      match l with
      | [] -> []
      | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
      | n::ns -> let (l1, l2)= split l in merge(mergesort(l1), mergesort(l2))



