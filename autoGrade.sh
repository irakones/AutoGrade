#!/bin/bash
regex="([0-9]+-[0-9]+)[[:space:]]-[[:space:]](([a-zA-Z][[:space:]]?)+)*"

touch "outcomes.csv"
tempDir="$PWD/Submissions/tmp"
mkdir -p $tempDir
mkdir -p Completed_Submissions
cp "$PWD/a1Checker.fsx" $tempDir

for entry in "$PWD/Submissions"/*
do
    if [[ -f $entry && $entry =~ $regex ]]; then
        #parse student name, and mycourses id from filename
        mycourses_id=${BASH_REMATCH[1]}
        name=${BASH_REMATCH[2]}
        echo "$mycourses_id $name"
        
        cp "$entry" "$tempDir/assignment1.fs"
       
        #remove existing module name and add the correct one
        dos2unix "$tempDir/assignment1.fs"
        sed -i '.bak' '/module/d' "$tempDir/assignment1.fs"
        sed -i '.bak' '1i\
            module hw1\
            ' "$tempDir/assignment1.fs" 

        numGraded=$(wc -l < outcomes.csv)
        fsharpi a1Checker.fsx "$mycourses_id" "$name"
        newNumGraded=$(wc -l < outcomes.csv)
        
        #if test failed, write to csv. Otherwise, move file to
        #graded_submissions
        if [[ "$newNumGraded" == "$numGraded" ]]; then
            echo "$mycourses_id,$name,TEST_FAIL" >> outcomes.csv
        else
            mv "$entry" Completed_Submissions
        fi
        rm "$tempDir/assignment1.fs"
    fi
done
rm -r "$tempDir"
