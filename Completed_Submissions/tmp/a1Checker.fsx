#r "FsCheck/FsCheck.dll"
#load "props.fsx"
open Props
open FsCheck

let getTestOutcome prop score = 
        try
            Check.QuickThrowOnFailure prop;
            "pass," + (string score)
        with
             _ -> "fail,"
        
let testOutcomes = [
    getTestOutcome propSumList 4; 
    getTestOutcome propPairLists 4; 
    getTestOutcome propWeightedMean 7;
    getTestOutcome propMemberOf 6;
    getTestOutcome propRemove 6;
    getTestOutcome propFindMax 13;
    getTestOutcome propSelSort 20;
    getTestOutcome propCommon 15;
    getTestOutcome propMerge 5;
    getTestOutcome propMergeSort 20] 

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
