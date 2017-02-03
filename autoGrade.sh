#!/bin/bash
regex="([0-9]+-[0-9]+)[[:space:]]-[[:space:]](([a-zA-Z][[:space:]]?)+)*"

tempDir="$PWD/Submissions/tmp"
mkdir -p $tempDir
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
        if [[ "$newNumGraded" == "$numGraded" ]]; then
            echo "$mycourses_id,$name,TEST_FAIL" >> outcomes.csv
        fi
        rm "$tempDir/assignment1.fs"
    fi
done
rm -r "$tempDir"
