#!/bin/bash

#SBATCH --job-name=base_result

## Specify the needed settings from the server
#SBATCH --mem-per-cpu=20G  # amount of memory the job requires, default is 2G  # memory per CORE

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_noCsub_noExtreme/err_out/%x_%A_%a.out # output 
#SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_noCsub_noExtreme/err_out/%x_%A_%a.err # error 

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

# Loop through each Cluster_N folder
for cluster_number in {1..25}; do
  cluster="Cluster_${cluster_number}"
  # Loop through each Factor_M subfolder inside the Cluster_N folder
  for factor_number in {6..11}; do
    factor="${cluster}/Factor_${factor_number}"
    folder_path="Cluster_${cluster_number}/Factor_${factor_number}"
    # Find the files ending with *base.txt and pass their paths to the R script
    for base_path in ${factor}/*base.txt; do
      if [ -e "$base_path" ]; then
        # Matching any file ending with _DISPres1.txt in the same folder as *base.txt
        disp_path="${factor}/*_DISPres1.txt"
        Rscript CMD_PMF_base.R "$base_path" "$cluster_number" "$factor_number" "$folder_path" "$disp_path"
      fi
    done
  done
done
