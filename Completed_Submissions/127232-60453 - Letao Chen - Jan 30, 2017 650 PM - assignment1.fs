(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Letao Chen, Id Number: 260686394 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce helperiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)

(* module hw1_sol.  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)

    // use pattern matching to take the sum of the list recursively
    // args: list 'l' of floats
        // terminating condition: list is empty
let rec sumlist l = 
    match l with
    | [] -> 0.0
    | x :: xs ->
        let sum = sumlist xs
        x + sum

// code for testing
(*
    let testlist1 = [1.0 .. 5.0]
    let sumtl1 = sumlist testlist1
*)        

    // assuming given 2 lists have the same length
        // else -> FAILURE 
    // match item i in l1 with item i in l2 recursively
        // teminating condition: l1 empty; therefore, l2 empty
let rec pairlists twolists =
  match twolists with
    | ([], []) -> []
    | ([], x :: xs) -> 
        failwith "Error -- lists are not of the same length"
    | (x :: xs, []) -> 
        failwith "Error -- lists are not of the same length"
    | (x :: xs, y :: ys) -> 
        // jan 16 -- deleted comment (jan 19) -- 
        (x, y) :: (pairlists (xs, ys))

// testing code
(*
let testlist2 = [2.0 .. 6.0]
let pairtl1tl2 = pairlists (testlist1, testlist2)
*)

    // given list of weights and data 
        // compute the mean 
        // implement myfold method - similar to one seen in class 
        // myfold implementation now takes in 2 lists 
    // using myfold, compute the numerator of the the mean 
        // summation from i to n of xi * wi 
    // compute the demoninator
        // summation from i to n of wi 
let w_mean weights data = 
    let rec myfold f v (w, d) = 
        match (w, d) with
        | ([],[]) -> v
        | (x :: xs, y :: ys)-> 
            myfold f (f v (x, y)) (xs, ys)

    // jan 16 -- deleted comment (jan 19) -- 
    let numerator = myfold (fun mu (x, y) -> mu + x*y) 0.0 (weights, data)
    let denom = myfold (fun mu (x, y) -> mu + x) 0.0 (weights, data)
    numerator / denom

// testing code
(*
let X = [1.0 .. 5.0]
let W = [10.0 .. -1.0 .. 6.0]
let w_mean_test = w_mean W X
*)
  
(* Question 2. *)  (* Do not edit this line. *)

    // check if 'a' is in given list 
        // recursively more through the list if 'a' != 'x' 
        // termination condition: whole list checked (now empty) -> false 
let rec memberof pair = 
    match pair with
    | (a, []) -> false
    | (a, x :: xs) ->
        if(a = x) then
            true
        else
            memberof(a, xs)

// completed: jan 19
// testing code 
(* 
let pair1 = (1, [1; 2; 3])
let testp1 = memberof pair1

let pair2 = (1, [])
let testp2 = memberof pair2

let pair3 = (1, [2; 3; 4])
let testp3 = memberof pair3
*)

    // given 'a', remove all instances of 'a' in given list recursively
    // termination condition: list empty
        // if 'a' != 'x', keep element x in the returned list using :: 
let rec remove(item, lst) = 
    match(item, lst) with
    | (a, []) -> []
    | (a, x :: xs) ->
        if(a = x) then
            remove(a, xs)
        else    
            x :: remove(a, xs)

// completed: jan 19            
// testing code 
(*
let pair4 = (2, [1; 3; 2; 4])
let testp4 = remove pair4
*)


(* Question 3. *)  (* Do not edit this line. *)

// given list 'l', recursively find the max value in the list 
let findMax l = 
    let rec helper(l,m) = 
        match l with
        | [] -> m
        | x :: xs ->
            if(x > m) then  
                helper(xs, x)
            else 
                helper(xs, m)

    match l with
    | [] -> failwith "Error -- empty list"
    | (x::xs) -> helper(xs,x)

// testing code
(*
let testlist3 = [1; 17; 3; 6; 1; 8; 3; 11; 6; 5; 9]
let maxtl3 = findMax testlist3
*)

// completed: jan 19


(* Question 4. *)  (* Do not edit this line. *)
  
// pick largest element
    // put in front
// rest of list recursively sorted
    // after removing largest element from list
// result: list in descending order (NO DUPLICATES)
let rec selsort l = 
    if l = [] then
        []
    else 
        let max = findMax l
        let nlist = remove(max, l)
        max :: selsort nlist

// testing code 
(*
let testSS = selsort([1 .. 2 .. 25] @ [1 .. 5 .. 21])
*)


(* Question 5. *)  (* Do not edit this line. *)

// given 2 lists 
    // for each 'x' in l1, check if x is in l2 
    // do recursively
    // lists do not have to be the same size 
        // terminating case: when one or both of the lists are empty 
let rec common twolists = 
    // lets use memberof method
    match twolists with
    | ([], []) -> []
    | ([], x :: xs) -> []
    | (x :: xs, []) -> []
    | (x :: xs, b) -> 
        if(memberof(x, b)) then
            x :: common(xs, b)
        else
            common(xs, b)

// testing code
(*
let testCommon = common([3; 4; 5; 7; 2], [1; 3; 5; 7; 9; 1])
*)

// completed: jan 19


(* Question 6. *)   (* Do not edit this line. *)

// from A1 file
(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

// helper function -- ignore 
    // append: '@' implementation
let rec append(l1, l2) = 
    match l1 with
    | [] -> l2
    | x :: xs -> x :: (append(xs, l2))

// helper function -- ignore
    // rev: reverse given list
    // use helper method: append     
let rec rev l = 
    match l with
    | [] -> []
    | x :: xs -> append(rev(xs), [x])

// given list 'l'
// for each pair of items, split item 'x1' into l1 and 'x2' in l2 recursively
let rec split l = 
    let revlist = rev l
    let rec helper l sublist1 sublist2 =
        match l with
            | [] -> (sublist1, sublist2)
            | [a] -> (a :: sublist1, sublist2)
            | x1 :: x2 :: xs ->
                helper xs (x2 :: sublist1) (x1 :: sublist2)
                
    helper revlist [] []

// test split
(*
let list2Split = ([15 .. -2 .. 1])
let tsplit = split list2Split
*)

// completed: jan 19

// given 2 lists 
// merge the list recursively
    // if 'x' in l1 < 'y' in l2
    // do x :: return list 'result'
    // call helper method on tail of l1, 'y' not used yet, so give entire l2 
    // termination condition: both lists empty; all items used
        // return 'result' of merge
let rec merge twolists = 
    let rec helper (l1, l2) result =
        match (l1, l2) with
        | ([], []) -> result
        | (x :: xs, []) -> 
            helper ([], xs) (x :: result)
        | ([], x :: xs) ->
            helper ([], xs) (x :: result)
        | (x :: xs, y :: ys) ->
            if(x < y) then 
                helper (xs, l2) (x :: result)
            else 
                helper (l1, ys) (y :: result)

    rev(helper twolists [])

// test merge
(*
let mergetl = merge tsplit
*)

// todo: jan 19 - confirm merge method works by finishing mergesort
// completed: jan 19

// given list 'l', do mergesort
    // recursively split list into lists of size 1
    // then call merge 
let rec mergesort l = 
    match l with
    | [] -> []
    | (n :: []) -> n :: [] (* Without this you will go into an infinite loop. *)
    | a ->
        let (sublist1, sublist2) = split a
        merge(mergesort sublist1, mergesort sublist2)

// test mergesort
(*
let testMS = mergesort [15 .. -2 .. 1]
*)
// completed: jan 19
