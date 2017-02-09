(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Samy Coulombe, Id Number: 260577454 *) (* Edit this line. *)

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
    | x::xs -> 
        (x + sumlist xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> 
        (x,y)::pairlists(xs,ys)

let w_mean weights (data:float list) =  
    let listTuple = pairlists (weights, data)
    listTuple
    let rec multiply listTuple = 
        match listTuple with
        | (x,y)::rest ->
            (x * y) + multiply(rest)
        | (_) -> 0.0
    let sumOfWeights = sumlist weights
    let weightedData = multiply listTuple
    weightedData / sumOfWeights

(* testing block for Question 1
let weightList = [8.88;7.77;6.66;5.0]
let dataList = [44.3;100.0;4.5;99.9]
w_mean weightList dataList *)        
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair = 
    let elementInPair = fst(pair)
    let listInPair = snd(pair)
    match listInPair with
    | [] -> false
    | x::xs -> 
        if x = elementInPair then true
        else memberof (elementInPair,xs)


let rec remove(item, lst) = 
    match lst with
    | [] -> []
    | x::xs ->
        if x = item then
            remove(item, xs)
        else 
            x::remove(item, xs)
(* test block for Question 2
memberof (1,[11;2;1])
remove("John", ["Jo";"Joey";"Johnathan";"James";"Jessie";"John"]) *)

(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
    let rec helper(l,m) = 
        match l with
        | [] -> m
        | x::xs -> 
            if (x > m) then helper(xs, x)
            else helper(xs, m)
        
    match l with
    | [] -> failwith "Error -- empty list"
    | x::xs -> 
        helper(xs,x)
    
(* test block for Question 3 
let testList = [1;1;2;3;2;3;2;3;4;5;7] 
let emptytestList = [] // doesn't work well when the list is empty
findMax testList *)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = 
    match l with
    | [] -> []
    | x::xs ->
        let currentMax = findMax(x::xs)
        let newList = remove(currentMax, x::xs)
        let finalSortedList = currentMax::(selsort newList)
        finalSortedList
(* test block for Question 4 
selsort([1;4;3;5;67;3;54;3;56;7;5;3;5;6;7;0;4;3;3;45;5;67;7]) *)

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists = 
    match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> []
    | (x::xs, []) -> []
    | (x::xs, y::ys) -> 
        if memberof (x, y::ys) then  
            x::(common (xs, remove(x,y::ys)))
        else common(xs, y::ys)
(* test block for Question 5 
common ([9;7;7;8;0;0;8;5;3],[3;4;5;0;0;1;1;7;2]) *)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let rec split l = 
    let rec erase (element, list) = // helper function that simply erases an element from the list
        match list with 
        | [] -> []
        | x::xs -> 
            if x = element then xs
            else x::(erase (element, xs))

    let rec splitHelper list1 = 
        match list1 with
        | [] -> []
        | x::xs ->
            if xs <> [] then
                let nextList = erase(xs.Head, xs)
                x::(splitHelper nextList)
            else   
                let endList = [x]
                endList
    
    let firstList = splitHelper(l)

    let evenElementsList = erase(l.Head, l)

    let secondList = splitHelper(evenElementsList)   
    firstList, secondList
        
    

(* test block for Question 6 
let testlist = ["a";"b";"c";"d";"e";"f"]
split testlist *)


let rec merge twolists = 
    let l1 = fst(twolists)
    let l2 = snd(twolists)
    match l1, l2 with
    | [],[] -> []
    | x::xs, [] -> 
        let biggestX = findMax(x::xs)
        let newl1 = remove(biggestX, x::xs)
        merge(newl1, [])@(biggestX::[])
    | [], y::ys ->
        let biggestY = findMax(y::ys)
        let newl2 = remove(biggestY, y::ys)
        merge([], newl2)@(biggestY::[])
    | x::xs, y::ys ->
        let biggestX = findMax(x::xs)
        let biggestY = findMax(y::ys)
        if biggestX > biggestY then
            let newl1 = remove(biggestX, x::xs)
            merge(newl1, y::ys)@(biggestX::[])
        elif biggestX < biggestY then  
            let newl2 = remove(biggestY, y::ys)
            merge(x::xs, newl2)@(biggestY::[])
        else    
            let newl1 = remove(biggestX, x::xs)
            let newl2 = remove(biggestY, y::ys)
            merge(newl1, newl2)@(biggestX::[])
        

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> // failwith "Not implemented"
        let firstHalf = fst(split(n::ns))
        let secondHalf = snd(split(n::ns))
        let mergesortedFirstHalf = mergesort(firstHalf)
        let mergesortedSecondHalf = mergesort(secondHalf)
        merge(mergesortedFirstHalf, mergesortedSecondHalf)
    

(* test block for Question 6 
let buggylist = [5;5;5;5;5;5;5;2;4;4;55;5;5;5;5;1]
let thisList = [5;5;5;5;5;5;5;2;4;4;55;5;5;5;5;99;99]
mergesort(thisList) *)


(* miscellaneous test block 
let dataList = [8.88;7.77;6.66;5.0]
let weightList = [44.3;101.0;4.5;45.0;4.5;99.9]
//w_mean weightList dataList
//pairlists (dataList, weightList)
let emptylist = []
let someOtherList = []
// selsort weightList
let listuno = [1;2;3;1]
let listdos = [1;10;0;1;3]
common (listuno, listdos)
// w_mean weightList emptylist
// w_mean weightList [1.0;2.2;0.33] *)