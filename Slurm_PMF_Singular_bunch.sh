#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=PMF_nonGUI

## Specify the needed settings from the server
#SBATCH --nodes=1  # number of nodes
#SBATCH --tasks-per-node=1  # tasks per node # up to 128;
#SBATCH --mem-per-cpu=20G  # amount of memory the job requires, default is 2G  # memory per CORE

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_txt_noCsub_noExtreme/err_out/%x_%j_%a.out # output file
#SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_txt_noCsub_noExtreme/err_out/%x_%j_%a.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu

## Specify the maximum of time in need
#SBATCH --time=05-00:00  # Total time needed for job: Days-Hours:Minutes

## create an array of jobs
#SBATCH --array=1-150

#load modules
module load gnu10/10.3.0-ya
module load r
module load singularity

# set the LANG environment variable
export LANG=C.UTF-8

# script to exit immediately if a command exits with a non-zero status
set -x

# Singularity container set-up
SINGULARITY_BASE=/containers/hopper/Containers
CONTAINER=${SINGULARITY_BASE}/wine/wine.sif
# -B ${PWD}:/host_pwd, bind a directory from the host system (outside the container) to a direc$
# here binding the current working directory (${PWD}) to the directory /host_pwd
# --pwd /host_pwd changes the current working directory to /host_pwd once inside the container.
# SINGULARITY_RUN="singularity exec  -B ${PWD}:/host_pwd --pwd /host_pwd"

# Calculate the Cluster and Factor numbers based on the array index
Cluster_number=$(( ( ($SLURM_ARRAY_TASK_ID - 1) / 6 ) + 1 ))
Factor_number=$(( ( ($SLURM_ARRAY_TASK_ID - 1) % 6 ) + 6 ))
echo ${Cluster_number}
echo ${Factor_number}

# Set up the files path for the ME-2.exe and the key file
ShR_SCRIPT_DIR="/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_txt_noCsub_noExtreme"
BASE_SCRIPT_DIR="$ShR_SCRIPT_DIR/Cluster_${Cluster_number}/Factor_${Factor_number}"

SINGULARITY_RUN="singularity exec  -B ${BASE_SCRIPT_DIR}:/host_pwd --pwd /host_pwd"
SCRIPT=PMF_bs_6f8xx_sealed_GUI_MOD.ini

# Define the DOS command to be used
# DOS_COMMAND="${SINGULARITY_RUN} ${CONTAINER} wine ${PWD}/ME-2.exe ${SCRIPT}"
DOS_COMMAND="${SINGULARITY_RUN} ${CONTAINER} wine ${BASE_SCRIPT_DIR}/ME-2.exe ${SCRIPT}"

# Calculate the Cluster and Factor numbers based on the array index
# Cluster_number=$(( ( ($SLURM_ARRAY_TASK_ID - 1) / 6 ) + 1 ))
# Factor_number=$(( ( ($SLURM_ARRAY_TASK_ID - 1) % 6 ) + 6 ))
# echo ${Cluster_number}
# echo ${Factor_number}

# Set the directory path for the Cluster and Factor folders
cd "Cluster_${Cluster_number}/Factor_${Factor_number}"
cp ../*.csv .
# cp ${BASE_SCRIPT_DIR}/${SCRIPT} .
cp ${ShR_SCRIPT_DIR}/me2key.key .
cp ${ShR_SCRIPT_DIR}/PMF_ab_base.dat .
cp ${ShR_SCRIPT_DIR}/ME-2.exe .
cp ${ShR_SCRIPT_DIR}/PMF_bs_6f8xx_sealed_GUI_MOD.ini .
echo "Key&exe copy finished"

# load modules again for each sub-task
# module load r
# module load singularity

## Run the tasks

# 1. Run DOS command for PMF base model analysis
# cp iniparams_base.txt iniparams.txt
cp iniparams_base_C_${Cluster_number}_F_${Factor_number}.txt iniparams.txt
echo "Base parameter file changed"
echo $DOS_COMMAND
echo $PWD
$DOS_COMMAND
rm iniparams.txt
echo "Base Model Run completes"

# 2. Analyze the output .txt file, generate the new value for numoldsol, and replace it in other iniparams.txt series using R
mv CSN_noCsub_noExtreme_C_${Cluster_number}_F_${Factor_number}_.txt CSN_noCsub_noExtreme_C_${Cluster_number}_F_${Factor_number}_base.txt
# Rscript ${BASE_SCRIPT_DIR}/minQ_Task_numoldsol.R CSN_C_${Cluster_number}_F_${Factor_number}_base.txt
Rscript ${ShR_SCRIPT_DIR}/minQ_Task_numoldsol.R CSN_noCsub_noExtreme_C_${Cluster_number}_F_${Factor_number}_base.txt ${Cluster_number} ${Factor_number} ${BASE_SCRIPT_DIR}
# ${Cluster_number} ${Factor_number} will pass the cluster/factor number as additional arguments to the R script
# cat CSN_C_${Cluster_number}_F_${Factor_number}_lowest_Qm_task.txt # print the lowest_Qm, already conducted in R
echo "minQ changed"

# 3. Run DOS command for BS, DISP, and BS-DISP analyses in turn
for param_file in iniparams_BS_C_${Cluster_number}_F_${Factor_number}_use.txt iniparams_DISP_C_${Cluster_number}_F_${Factor_number}_use.txt
# for param_file in iniparams_BS_C_${Cluster_number}_F_${Factor_number}.txt iniparams_DISP_C_${Cluster_number}_F_${Factor_number}.txt
# for param_file in iniparams_BS.txt iniparams_DISP.txt # iniparams_BS_DISP.txt
do
  cp ${param_file} iniparams.txt
  $DOS_COMMAND
  rm iniparams.txt
done

# Rename the output from DISP
mv DISPres1.txt CSN_C_${Cluster_number}_F_${Factor_number}_DISPres1.txt

echo "All runs executed!"