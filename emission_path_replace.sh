#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=emission_path_sumaiya

## Specify the needed settings from the server
#SBATCH --nodes=1  # number of nodes
#SBATCH --tasks-per-node=1  # tasks per node # up to 128;
#SBATCH --mem-per-cpu=20G  # amount of memory the job requires, default is 2G  # memory per CORE

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/projects/HAQ_LAB/tzhang/haq_work/sumaiya_cmaq/%x_%j_%a.out # output file
#SBATCH --error=/projects/HAQ_LAB/tzhang/haq_work/sumaiya_cmaq/%x_%j_%a.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

## Specify the maximum of time in need
#SBATCH --time=05-00:00  # Total time needed for job: Days-Hours:Minutes

# script to exit immediately if a command exits with a non-zero status
set -x

###### Locate .ncf and .ncf.gz Files and combine

# Folder A where .ncf and .ncf.gz files are located
# source_nc="/groups/ESS/schang31/eq/EQUATES_2011/premerged/"
source_nc="/groups/ESS/schang31/eq"

# Output file to store paths of relevant .ncf and .ncf.gz files
equates_path_nc="/projects/HAQ_LAB/tzhang/haq_work/sumaiya_cmaq/equates_path_nc.txt"

# Initialize the output file
> "$equates_path_nc"

# Search for relevant .ncf and .ncf.gz files in all subfolders under premerged
# find "$source_nc" -type d -name "premerged" -exec find {} -type f \( -name '*_date_12US1_*_20[0-2][0-9].ncf' -o -name '*_date_12US1_*_20[0-9][0-9].ncf.gz' \) \; >> "$equates_path_nc"

# Loop through subfolders to find .ncf or .ncf/.gz files
for equates_folder in "$source_nc"/EQUATES_20[0-2][0-9]; do
    if [[ -d "$equates_folder" ]]; then
        premerged_folders=("$equates_folder"/premerged/*)
        for premerged_folder in "${premerged_folders[@]}"; do
            if [[ -d "$premerged_folder" ]]; then
                # Search for relevant .ncf and .ncf.gz files in subfolders under premerged
                find "$premerged_folder" -type f \( -name '*_date_12US1_*_20[0-9][0-9].ncf' -o -name '*_date_12US1_*_20[0-9][0-9].ncf.gz' \) >> "$equates_path_nc"
            fi
        done
    fi
done


###### Locate .txt files with dates and combine them

# Folder where .txt files are located
date_txt="/groups/ESS3/share/projects/emis_dates"

# Output file to store combined .txt files
source_dates="/projects/HAQ_LAB/tzhang/haq_work/sumaiya_cmaq/emission_source_dates.txt"

# Initialize the output file
> "$source_dates"

# Loop through subfolders in folder B
for year_folder in "$date_txt"/20[0-2][0-9]; do
    if [[ -d "$year_folder" ]]; then
        # Find and concatenate .txt files in each year folder and append to the output_txt_file
        find "$year_folder" -type f -name 'smk_merge_dates_*.txt' -exec cat {} >> "$source_dates" \;
    fi
done

##### Map dates to file paths and replace in txt
module load r

# Path to your R script for date mapping and replacement
R_date_emission_path="/projects/HAQ_LAB/tzhang/haq_work/sumaiya_cmaq/R_date_emission_path.R"

# Execute the R script
Rscript "$R_date_emission_path"



