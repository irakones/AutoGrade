#r "FsCheck/FsCheck.dll"
#load "props.fsx"
open Props
open FsCheck

let getTestOutcome prop = 
        try 
            Check.QuickThrowOnFailure prop; "pass"
        with
             _ -> "fail"
        
let testOutcomes = [
    getTestOutcome propSumList; 
    getTestOutcome propPairLists; 
    getTestOutcome propWeightedMean;
    getTestOutcome propMemberOf;
    getTestOutcome propRemove;
    getTestOutcome propFindMax;
    getTestOutcome propSelSort;
    getTestOutcome propCommon;
    getTestOutcome propMerge;
    getTestOutcome propMergeSort] 

let outcomesString = List.fold (fun a x -> a + x+ ",") "" testOutcomes
let args = fsi.CommandLineArgs
let gradeLine = args.[1] + "," + args.[2] + "," + outcomesString + "\n"

let inFile = new System.IO.StreamReader("outcomes.csv") in 
let inText = inFile.ReadToEnd()
inFile.Close()
let outText = inText + gradeLine

let wr = new System.IO.StreamWriter("outcomes.csv") in 
wr.Write(outText)
wr.Flush()
