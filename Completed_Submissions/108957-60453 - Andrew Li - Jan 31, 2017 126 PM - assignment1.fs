(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Andrew Li, Id Number: 260629290 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)

(* module hw1_sol. Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)

let rec sumlist (l: float list) =
  match l with
    | [] -> failwith "Error -- input list was empty"
    | [x] -> x
    | x::xs -> x + sumlist xs                        // Recursively call to sum up floats in list

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: pairlists (xs,ys)   // Returns a list of pairs

// w_mean calculates the weighted mean of a sequence of real numbers
let w_mean (weights: float list) (data: float list) =
  let pairedlist = pairlists (weights,data)

  (* The pairproduct function takes the list of paired floats
     Then it adds up the products obtained from each pair *)
  let rec pairproducts plist =
    match plist with
    | [] -> failwith "Error -- input paired list was empty"
    | [(x,w)] -> (x * w)
    | (x,w)::tail -> (x * w) + pairproducts (tail)
    
  (* If the sum of the weights is 0.0, then display Error
     Precaution despite assuming weights are positive reals *)
  if (sumlist weights) <> 0.0 then
    (pairproducts pairedlist)/(sumlist weights)
  else failwith "Error -- division by 0"
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
  match pair with
  | (key, []) -> false
  | (key, x::xs) -> if key = x then true
                    else memberof (key, xs)    // Check if next item is the key

let rec remove(item, lst) =
  match lst with
  | [] -> []
  | x::xs -> if x = item then remove(item, xs) // Recursively remove item
             else x :: (remove(item, xs))      // Leave item in list and check for next element in list


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) =                        // Helper function compares values and returns largest when
    match l with                               // no more values left to compare
    | [] -> m                                  // Return maximum value
    | x::xs -> if x > m then helper(xs, x)     // If x > m, then compare with x instead
               else helper(xs, m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l =
  match l with
  | [] -> []
  | [x] -> [x]
  | x::xs -> if x = findMax l then x::selsort(remove(x,xs)) // Remove largest element, selection sort remaining list
             else selsort(xs @ [x])                         // Call selection sort appending current x to tail of list

(* Question 5. *)  (* Do not edit this line. *)

// Common function produces a list of elements that is a member in both lists
let rec common twolists =
  match twolists with
  | ([], []) -> []
  | (x::xs, []) -> []
  | ([], y::ys) -> []
  | (x::xs, y::ys) -> if memberof (x, y::ys) then x::common(remove (x,xs), remove (x, y::ys)) 
                      else common(remove (x, xs), y::ys) // If x not in the second list, remove all occurrences from xs list

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
  match l with
  | [] -> ([], [])                        // Base Case: when empty list, return a pair of empty lists
  | x::xs ->
    match split xs with                   // Match result from recursive call on split xs
    | ([], []) -> ([x], [])
    | (y::ys, []) -> ([x], y::ys)
    | ([], y::ys) -> ([x], y::ys)         // Satisfies F# incomplete pattern matching warning
    | (y::ys, z::zs) -> (x::z::zs, y::ys) // Alternate lists to append x into list

let rec merge twolists =
  match twolists with
  | ([], []) -> []                        // Case where both lists are empty
  | (x::xs, []) -> x::xs                  // Cases where remaining elements are only in one list
  | ([], y::ys) -> y::ys
  | (x::xs, y::ys) -> if x < y then x::(merge (xs, y::ys)) // Organizes in ascending order
                      else y::(merge (x::xs, ys))

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns ->
    match split l with                    // Merges lists after being split
    | ([], []) -> []
    | (x::xs, []) -> x::xs
    | ([], y::ys) -> y::ys
    | (x::xs, y::ys) -> merge(mergesort (x::xs), mergesort (y::ys)) // Recursively merge mergesorted lists

