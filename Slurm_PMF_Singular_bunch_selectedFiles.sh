#!/bin/bash

#SBATCH --partition=normal
#SBATCH --job-name=PMF_nonGUI

## Specify the needed settings from the server
#SBATCH --nodes=1  # number of nodes
#SBATCH --tasks-per-node=1  # tasks per node # up to 128;
#SBATCH --mem-per-cpu=20G  # amount of memory the job requires, default is 2G  # memory per CORE

## Specify the maximum of time in need
#SBATCH --time=05-00:00  # Total time needed for job: Days-Hours:Minutes

## create an array of jobs
#SBATCH --array=1-150

# Select specific nodes, those potentially more efficient
### SBATCH --nodelist=hop051,hop053,hop054,hop055,hop059,hop061,hop064,hop065,hop066,hop072,hop073

## Assign the name of job, output & error files
## NOTE: %u=userID, %x=jobName, %N=nodeID, %j=jobID, %A=arrayID, %a=arrayTaskID
#SBATCH --output=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_noCsub_noExtreme_2/err_out/%x_%j_%A_%a.out # output file
#SBATCH --error=/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_noCsub_noExtreme_2/err_out/%x_%j_%A_%a.err # error file

## Email info for updates from Slurm
#SBATCH --mail-type=BEGIN,END,FAIL # ALL,NONE,BEGIN,END,FAIL,REQUEUE,..
#SBATCH --mail-user=tzhang23@gmu.edu



#load modules
module load gnu10/10.3.0-ya
module load r
module load singularity

# set the LANG environment variable
export LANG=C.UTF-8

# script to exit immediately if a command exits with a non-zero status
set -x

#### Basic setup ####
# Singularity container set-up
SINGULARITY_BASE=/containers/hopper/Containers
CONTAINER=${SINGULARITY_BASE}/wine/wine.sif

# Calculate the Cluster and Factor numbers based on the array index
Cluster_number=$(( ( ($SLURM_ARRAY_TASK_ID - 1) / 6 ) + 1 ))
Factor_number=$(( ( ($SLURM_ARRAY_TASK_ID - 1) % 6 ) + 6 ))
echo ${Cluster_number}
echo ${Factor_number}

# Set up the files path for the ME-2.exe and the key file
ShR_SCRIPT_DIR="/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_noCsub_noExtreme_2"
BASE_SCRIPT_DIR="$ShR_SCRIPT_DIR/Cluster_${Cluster_number}/Factor_${Factor_number}"

# Check if the necessary files are present, stop the rest runs if present
use_txt=$(find ${BASE_SCRIPT_DIR} -type f -name "*_use.txt" | wc -l)
DISPres1=$(find ${BASE_SCRIPT_DIR} -type f -name "*_DISPres1.txt" | wc -l)
Qm_task=$(find ${BASE_SCRIPT_DIR} -type f -name "*_Qm_task.txt" | wc -l)
base_txt=$(find ${BASE_SCRIPT_DIR} -type f -name "*_base.txt" | wc -l)

echo "base_result.txt: " ${base_txt}
echo "Qm_task.txt: " ${Qm_task}
echo "use.txt: " ${use_txt}
echo "DISPres1.txt: " ${DISPres1}

if [ "$DISPres1" -eq 1 ] && [ "$use_txt" -eq 2 ]; then
  echo "Base files already present, skipping this run."
  exit 0
fi

# Set up singularity run
SINGULARITY_RUN="singularity exec  -B ${BASE_SCRIPT_DIR}:/host_pwd --pwd /host_pwd"
SCRIPT=PMF_bs_6f8xx_sealed_GUI_MOD.ini

# Define the DOS command to be used
DOS_COMMAND="${SINGULARITY_RUN} ${CONTAINER} wine ${BASE_SCRIPT_DIR}/ME-2.exe ${SCRIPT}"

# Set the directory path for the Cluster and Factor folders
#cd "Cluster_${Cluster_number}/Factor_${Factor_number}"
#cp ../*.csv .
#cp ${ShR_SCRIPT_DIR}/me2key.key .
#cp ${ShR_SCRIPT_DIR}/PMF_ab_base.dat .
#cp ${ShR_SCRIPT_DIR}/ME-2.exe .
#cp ${ShR_SCRIPT_DIR}/PMF_bs_6f8xx_sealed_GUI_MOD.ini .
#echo "Key&exe copy finished"


#### Run the tasks ####

##### 1. Run DOS command for BS, DISP, and BS-DISP analyses if there are base results #####

if [ "$Qm_task" -eq 1 ] && [ "$base_txt" -eq 1 ]; then

    # Run DOS command for BS, DISP, and BS-DISP analyses in turn
    for param_file in iniparams_BS_C_${Cluster_number}_F_${Factor_number}_use.txt iniparams_DISP_C_${Cluster_number}_F_${Factor_number}_use.txt
    do
      cp ${param_file} iniparams.txt
      $DOS_COMMAND
      rm iniparams.txt
    done

    # Rename the output from DISP
    mv DISPres1.txt CSN_C_${Cluster_number}_F_${Factor_number}_DISPres1.txt
    
    echo "All runs executed!"
    exit 0
fi

##### 2. Run DOS command if there is no base result #####

# 2.1 Run DOS command for Base Model 
cp iniparams_base_C_${Cluster_number}_F_${Factor_number}.txt iniparams.txt
echo "Base parameter file changed"
echo $DOS_COMMAND
echo $PWD
$DOS_COMMAND
rm iniparams.txt
echo "Base Model Run completes"

# 2.2 Analyze the output .txt file, generate the new value for numoldsol, and replace it in other iniparams.txt series using R
mv CSN_noCsub_noExtreme_C_${Cluster_number}_F_${Factor_number}_.txt CSN_noCsub_noExtreme_C_${Cluster_number}_F_${Factor_number}_base.txt
Rscript ${ShR_SCRIPT_DIR}/minQ_Task_numoldsol.R CSN_noCsub_noExtreme_C_${Cluster_number}_F_${Factor_number}_base.txt ${Cluster_number} ${Factor_number} ${BASE_SCRIPT_DIR}
echo "minQ changed"

# 2.3 Run DOS command for BS, DISP, and BS-DISP analyses in turn
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