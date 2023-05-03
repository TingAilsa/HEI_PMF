#!/bin/bash

## Specify the needed settings from the server 
#SBATCH --nodes=1  # number of nodes
#SBATCH --tasks-per-node=1  # tasks per node
#SBATCH --mem-per-cpu=10G  # amount of memory the job requires, default is 2G

## Assign the name of job, output & error files
#SBATCH --job-name=pmf_noGUI_try
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=pmf_noGUI_try_%N_%j.out # output file
#SBATCH --error=pmf_noGUI_try_%N_%j.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

## Specify the maximum of time in need
#SBATCH --time=03-00:00  # Total time needed for job: Days-Hours:Minutes

#load modules
module load singularity

# Define the DOS command to be used
DOS_COMMAND="ME-2 PMF_bs_6f8xx_sealed_GUI_MOD.ini"

# Set the directory path for the Cluster and Factor folders
cd "cd /projects/HAQ_LAB/tzhang/pmf_no_gui/file_try/PMF_no_GUI"

## Run the tasks

# PMF base model analysis
cp iniparams_base_1.txt iniparams.txt

# Run the MS-DOS command using Singularity and DOSBox
# Updated  DOSBox command to mount  current working directory as the virtual C: drive with mount c .
# singularity exec dosbox_container.sif dosbox -c "mount c ${input_dir}" -c "c:" 
singularity exec /projects/HAQ_LAB/tzhang/pmf_no_gui/file_try/dosbox_container.sif dosbox -c "mount c ." -c "c:" -c "$DOS_COMMAND" -c "exit"

rm iniparams.txt