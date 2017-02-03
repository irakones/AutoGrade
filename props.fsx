module Props
open FsCheck
#r "FsCheck/FsCheck.dll"
#load "hw1_sol.fs";;
#load "Submissions/tmp/assignment1.fs";;

//helpers for testing
let epsilon = 0.001
let close (x: float) (y: float) = abs(x - y) < epsilon


let rec separate l = 
    match l with 
    | [] -> ([], [])
    | (x,y)::zs -> let (l1, l2) = separate zs in (x::l1, y::l2)

let isSmall (x: NormalFloat) = 
    try
        abs (float x) < 1.340780793e+100
    with
        _ -> false

let isPos (x: NormalFloat) = 
    try
        abs (float x) > 0.0
    with
        _ -> false
        
let filterLarge lst = 
    List.filter isSmall lst

let floatList (l: NormalFloat list) = 
    List.map float (filterLarge l)

let filterLargePaired pairedList = 
    List.filter (fun (x, y) -> isSmall x && isSmall y && isPos x) pairedList
//Q1 tests

let propSumList (l: NormalFloat list) =  
    let floatd = floatList l in 
    close (hw1Sol.sumlist floatd) (hw1.sumlist floatd)

let propPairLists pairedLists = 
    let listPair = separate pairedLists in 
    hw1Sol.pairlists listPair = hw1.pairlists listPair

let propWeightedMean (l:(NormalFloat * NormalFloat) list) = 
    if List.isEmpty l then true else
    let (w, d) = separate (filterLargePaired l) in 
    let (fw, fd) = (floatList w, floatList d) in 
    close (hw1Sol.w_mean fw fd) (hw1.w_mean fw fd)

// Q2
let propMemberOf (x, xs) = 
    hw1Sol.memberof(x,xs) = hw1.memberof(x,xs)

let propRemove (x, xs) =
    hw1Sol.remove (x, xs) = hw1.remove(x, xs)

//Q3
let propFindMax (l: NormalFloat list) =
    let fl = floatList l in 
    if List.isEmpty fl then true else hw1Sol.findMax fl = hw1.findMax fl

//Q4
let propSelSort (l: NormalFloat list) = 
    let fl = floatList l in 
    hw1Sol.selsort fl = hw1.selsort fl

//Q5
let propCommon twoLists = 
    hw1Sol.common twoLists = hw1.common twoLists

//Q6
let propMerge (lists: NormalFloat list * NormalFloat list) =  
    hw1Sol.merge lists = hw1.merge lists

let propMergeSort (l: NormalFloat list) =
    let smallFloats = List.filter isSmall l in  
    hw1Sol.mergesort l = hw1.mergesort l 
