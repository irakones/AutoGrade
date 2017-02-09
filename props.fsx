module Props
open FsCheck
#r "packages/FsCheck/lib/net45/FsCheck.dll"
#load "hw1_sol.fs";;
#load "Submissions/tmp/assignment1.fs";;

//helpers for testing
let epsilon = 0.001
let close (x: float) (y: float) = abs(x - y) < epsilon

let smallFloatGen = 
    Gen.filter (fun x -> 
        not (System.Double.IsNaN x) && 
        not (System.Double.IsInfinity x)) (Gen.elements [-1000.0..0.01..1000.0])

let smallNotZeroFloatGen =
   Gen.filter (fun x -> abs x > 0.0) smallFloatGen

type SmallFloat = float
type SmallNotZeroFloat = float

type MyGens =
    static member SmallFloat() = 
        {new Arbitrary<double>() with
            override x.Generator = smallFloatGen}
    static member SmallNotZeroFloat() =
        {new Arbitrary<double>() with
            override x.Generator = smallNotZeroFloatGen}

Arb.register<MyGens>()

let rec separate l = 
    match l with 
    | [] -> ([], [])
    | (x,y)::zs -> let (l1, l2) = separate zs in (x::l1, y::l2)

//Q1 tests

let propSumList (l: SmallFloat list) = 
    close (hw1Sol.sumlist l) (hw1.sumlist l)

let propPairLists pairedLists = 
    let listPair = separate pairedLists in 
    hw1Sol.pairlists listPair = hw1.pairlists listPair

let propWeightedMean (l:(SmallNotZeroFloat * SmallFloat) list) = 
    if List.isEmpty l then true else
    let (ws, ds) = separate l in 
    close (hw1Sol.w_mean ws ds) (hw1.w_mean ws ds)

// Q2
let propMemberOf (x, xs) = 
    hw1Sol.memberof(x,xs) = hw1.memberof(x,xs)

let propRemove (x, xs) =
    hw1Sol.remove (x, xs) = hw1.remove(x, xs)

//Q3
let propFindMax (l: SmallFloat list) =
    if List.isEmpty l then true else hw1Sol.findMax l = hw1.findMax l

//Q4
let propSelSort (l: SmallFloat list) = 
    hw1Sol.selsort l = hw1.selsort l

//Q5
let propCommon twoLists = 
    hw1Sol.common twoLists = hw1.common twoLists

//Q6
let propMerge (lists: SmallFloat list * SmallFloat list) =  
    hw1Sol.merge lists = hw1.merge lists

let propMergeSort (l: SmallFloat list) =
    hw1Sol.mergesort l = hw1.mergesort l 

let propList: obj list = [
    propSumList
    propPairLists
    propWeightedMean
    propMemberOf
    propRemove
    propFindMax
    propSelSort
    propMerge
    propMergeSort
]

let weights = [4; 4; 7; 6; 6; 13; 20; 15; 5; 20]