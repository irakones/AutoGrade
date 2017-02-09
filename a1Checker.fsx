#r "/Users/irakones/Documents/W17/COMP302/A1/packages/FsCheck/lib/net45/FsCheck.dll"
#load "props.fsx"
open Props
open FsCheck
let getTestOutcome (score, prop) = 
        try
            Check.QuickThrowOnFailure prop
            "pass," + (string score)
        with
             _ -> "fail,"

let testOutcomes = 
    List.zip Props.weights Props.propList |> 
        List.map (fun (score, prop) -> getTestOutcome (score, prop))
let outcomesString = System.String.Join(",", testOutcomes)
let args = fsi.CommandLineArgs
let gradeLine = args.[1] + "," + args.[2] + "," + outcomesString + "\n"
let writeGrade (gradeLine: string) =
    use wr = System.IO.File.AppendText("outcomes.csv")
    wr.Write gradeLine
writeGrade gradeLine