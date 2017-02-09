(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Elliane Croce, Id Number: 260604465 *) (* Edit this line. *)

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

    (*returns sum of a list of floats as a float*)
    let rec sumlist l:float = 
        match l with
        |[] -> 0.0 
        |x::xs -> x + sumlist xs

    (*takes two lists of same length m, returns list of pairs (x1,y1) of length m*)
    let rec pairlists (l1, l2) =
        match (l1, l2) with
        | ([],[]) -> []
        | ([],x::xs) -> failwith "Error -- lists are not of the same length"
        | (x::xs, []) -> failwith "Error -- lists are not of the same length"
        | (x::xs, y::ys) -> (x,y)::pairlists(xs, ys)

    (*takes list of pairs, computes the product of each pair, and returns a list of their products*)
    let rec prodpairs l =
        match l with
        |[] -> []
        |(a:float,b:float)::xs -> (a*b)::prodpairs xs


    let w_mean weights data = 
        let pairs = pairlists (weights, data); (*pairs the weights and data*)
        let prodlist = prodpairs pairs; (*computes product of the pairs*)
        let a = sumlist prodlist; (*NUMERATOR sums the products*)
        let b = sumlist weights; (*DENOMINATOR sums the weights*)
        let miu = a/b; (*computes miu*)
        miu;

    (* Question 2. *)  (* Do not edit this line. *)

    (*checks if the list is empty, if the first element 
    of the list matches the desired y, and if not 
    re-searches the list without the first element.*)
    let rec memberof (y, lst) = 
        match lst with
        |[] -> false
        |x::xs when x = y -> true
        |x::xs -> memberof (y, xs)

    (*if the element matches the item, continue checking the rest of
    the list without appending the matched x, if not, append the element
    to the rest of the list and continue searching for the item*)
    let rec remove(item, lst) = 
        match lst with
        |[] -> lst
        |x::xs when x = item -> remove(item, xs)
        |x::xs -> x::remove(item, xs)

    (* Question 3. *)  (* Do not edit this line. *)

    let rec findMax l = 
    (*helper takes list l and max value m*)
        let rec helper(l,m) =  
            match l with
            |[] -> m (*if l is empty then m is the max value*)
            |x::xs when x > m -> helper(xs, x) (*if x is greater than current m, replace it*)
            |x::xs -> helper(xs, m) (*else just continue searching the list*)
        match l with
        |[] -> failwith "Error -- empty list"
        |x::xs -> helper(xs, x) (*calls the helper with the first element as the largest element, comparing to the rest of the list*)

    (* Question 4. *)  (* Do not edit this line. *)

    let rec selsort l = 
        let rec maxelmt (l,m) =
            match l with
            |[] -> m (*if l is empty then m is the max value*)
            |x::xs when x > m -> maxelmt(xs, x) (*if x is greater than current m, replace it*)
            |x::xs -> maxelmt(xs, m) (*else just continue searching the list*)

    (*uses remove function from previous question to take any iterations of the current item from the list*)
        let rec remove(item, lst) = 
            match lst with
            |[] -> lst
            |x::xs when x = item -> remove(item, xs)
            |x::xs -> x::remove(item, xs)

    (*finds max element, puts it at the front of the list, then sorts the rest of the list with the max element removed*)
        match l with
            |[] -> []
            |x::xs -> maxelmt(xs,x)::selsort(remove(maxelmt(xs,x), l))

    (* Question 5. *)  (* Do not edit this line. *)

    let rec common (l1, l2) = 

    (*checks if element y is a member of l2, if it is this function returns true*)
        let rec memberof (y, lst) = 
            match lst with
            |[] -> false
            |x::xs when x = y -> true
            |x::xs -> memberof (y, xs)

    (* if there are elements in both lists, it checks if all xs in l1 are elements in l2*)
        match (l1, l2) with
        | ([],[]) -> []
        | ([],x::xs) -> []
        | (x::xs, []) -> []
        | (x::xs, y::ys) ->
            if memberof(x, l2) then x::common(xs, l2) (*if x is in l2 then it adds it to the new list*)
            else common(xs, l2)

    (* Question 6. *)   (* Do not edit this line. *)

    (* Mergesort requires that you use recursion.  Using isort or
    some other sort defeats the whole purpose.*)

    let rec split l = 
        let rec innersplit k (xs, ys) =
            match k with
            |x::y::lst -> innersplit lst (x::xs, y::ys) (*take first to elements and append them respectively to xs and ys*)
            |[x] -> (x::xs, ys) (*if odd # elements, add last to first list*)
            |[] -> (xs, ys)
        innersplit l ([], []) (*starts innersplit with the given list and two empty lists to fill*)

    let rec merge (l1, l2) = 
        match (l1, l2) with
        |([], []) -> []
        |(x::xs, []) -> l1 (*if one list is empty, then return only filled list*)
        |([], x::xs) -> l2
        |(x::xs, y::ys) ->
            if x < y then x::merge(xs, l2) (*if the first element of xs is smaller than the first element in y, add it to the list and then recall the merge function with the shortened xs and the full ys*)
            else y::merge(l1, ys) (*else y is smaller and append that*)


    let rec mergesort l = 
        match l with
        | [] -> []
        | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
        | n::ns -> merge(split(l)) (*split the list then merge the two*)


