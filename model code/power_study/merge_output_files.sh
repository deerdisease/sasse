#!/bin/bash

#################
# configuration #
#################

# Directory containing sqlite3 files to merge
directory="output/power_study"

# Output directory
odir="output/power_study_merged"

# Output file name
oname="power_study_merged.sqlite3"

##############
# processing #
##############

# Create output directory
mkdir -p $odir

# output file
ofile="$odir/$oname"

# Pattern to match (change this to your desired pattern)
pattern="*.sqlite3"

# array with files matching the pattern, using IFS to account for possible 
# spaces in filenames
IFS="
"
files_to_merge=( $(ls $directory/$pattern) )
n_files_to_merge=${#files_to_merge[@]}

echo "Initialization file: ${files_to_merge[0]}"

# initialize merged file
cp "${files_to_merge[0]}" "$ofile"

# extract schema once, since it is assumed to be common across files
sqlite3 $ofile .schema > $odir/schema.sql

echo "Initialized output: $ofile"

# Loop over each file that matches the pattern
for((i=1; i<n_files_to_merge; i++))
do
    item=${files_to_merge[$i]}
    if [ -f "$item" ]; then  # Check if it's a file
        echo "Merging: $item"
        sqlite3 $item .dump > $odir/dump.sql
        grep -vx -f $odir/schema.sql $odir/dump.sql > $odir/data.sql
        sqlite3 $ofile < $odir/data.sql
    fi
done

# clean up temporary sql scripts
rm $odir/*.sql
