##clear environment
# rm(list=ls())

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_no_GUI/iniparams_templates")
getwd()
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp"

##packages in need
library(tidyr) 
library(stats) 
library(ggplot2)
library(scales) 
library(dplyr)
library(plyr)


#### A. create cluster & sub-factor folders, DONE!!!  ####
## set path for all new folders
# pathway = paste0(data.dir,"/CSN_CMD_txt/")
# pathway = paste0(data.dir,"/CSN_CMD_txt_noCsub/")
# pathway = paste0(data.dir,"/CSN_CMD_txt_noCsub_noExtreme/")

## name of folders and sub-folders
clusterNum = paste0("Cluster_", 1:25)
factorNum = paste0("Factor_", 6:11)

## create new folders
create_dir(pathway, clusterNum, factorNum)


#### B1 start. read iniparams files  ####
base_par_org = readLines("iniparams_base_1.txt")
BS_par_org = readLines("iniparams_BS_2.txt")
DISP_par_org = readLines("iniparams_DISP_3.txt")
before_BS_DISP_par_org = readLines("iniparams_BS_PHASE_of_BSDISP_4.txt")
BS_DISP_par_org = readLines("iniparams_BS-DISP_5.txt")

cluster.NO = 1:25
factor.number = c(6:11)

#### B2. edit and output new iniparams.txt ####

### All data, All Variables!
cluster_sum = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files/National_SA_PMF/PMF_NoGUI_cluster/CSN_PMF_CMD_StrongWeakBad_Cluster.csv")
pathway = paste0(data.dir,"/CSN_CMD_txt/")
name.prefix = "CSN_C_" # prefix in names for input/output files

### All data, NO OC/EC Subgroups!
cluster_sum = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files/National_SA_PMF/PMF_NoGUI_NoCsub_cluster/CSN_noCsub_PMF_CMD_StrongWeakBad_Cluster.csv")
pathway = paste0(data.dir,"/CSN_CMD_txt_noCsub/")
name.prefix = "CSN_noCsub_C_" # prefix in names for input/output files

### No extreme values, NO OC/EC Subgroups
cluster_sum = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files/National_SA_PMF/PMF_NoGUI_NoCsub_NoExtreme_cluster/CSN_noCsub_noExtreme_PMF_CMD_StrongWeakBad_Cluster.csv")
pathway = paste0(data.dir,"/CSN_CMD_txt_noCsub_noExtreme/")
name.prefix = "CSN_noCsub_noExtreme_C_" # prefix in names for input/output files

cluster_sum$X = NULL

for(i in 1:length(cluster.NO)){
  ## data of selected cluster
  cluster_info = cluster_sum[cluster_sum$Finaly.Decision == i, ]
  
  # specific info to use for the cluster
  cluster.row = cluster_info$cluster.row
  variable.NO = cluster_info$sum.weak.good
  ## weak0 or strong1 for PM2.5 species 
  wead.strong.assign = cluster_info$style.weak.good
  
  # change to the style used for txt
  # remove the first "/", and change the rest to "\t"
  wead.strong.assign = gsub("/", "\\\t", 
                            sub("^/", "", wead.strong.assign))
  
  # replace the number of factors to use and name of output files
  for (j in factor.number){
    base_par = base_par_org
    BS_par = BS_par_org
    DISP_par = DISP_par_org
    before_BS_DISP_par = before_BS_DISP_par_org
    BS_DISP_par = BS_DISP_par_org
    
    # output path
    path.CF = paste0(pathway, "Cluster_", i, "/Factor_", j, "/")
    
    # replace with the row, variable, & factor number 
    base_par = row_var_factor(base_par, cluster.row, variable.NO, j)
    BS_par = row_var_factor(BS_par, cluster.row, variable.NO, j)
    DISP_par = row_var_factor(DISP_par, cluster.row, variable.NO, j)
    before_BS_DISP_par = row_var_factor(before_BS_DISP_par, cluster.row, variable.NO, j)
    BS_DISP_par = row_var_factor(BS_DISP_par, cluster.row, variable.NO, j)

    ##### B1: iniparams for Base run #####
    # create and replace the names for base run input & output files

    base.input = paste0(name.prefix, cluster.NO[i], "_PMF_CMD.csv")
    output.pre = paste0(name.prefix, cluster.NO[i], "_F_", j)

    base.output = paste0(output.pre, "_")
    
    base_par = base_input_output(base_par, base.input, base.output)
    
    write.table(base_par, 
                file = paste0(path.CF, "iniparams_base_C_", i,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    ##### B2: iniparams common input for BS (bootstrap) & DISP files #####
    # create and replace the name of input file
    BS.DISP.input.1 = base.input
    BS.DISP.input.2 = paste0(output.pre, "_.dat")
    
    BS.output = paste0(output.pre, "_BS_")
    DISP.output = paste0(output.pre, "_DISP_")
    before_BS_DISP.output = paste0(output.pre, "_before_BS_DISP_")
    BS_DISP.output = paste0(output.pre, "_BS_DISP_")
    
    BS_par = bs_disp_input_output(BS_par, 
                                  BS.DISP.input.1, 
                                  BS.DISP.input.2, 
                                  BS.output)
    
    DISP_par = bs_disp_input_output(DISP_par, 
                                    BS.DISP.input.1, 
                                    BS.DISP.input.2, 
                                    DISP.output)
    DISP_par = dispbcmask_rp(DISP_par, 
                             wead.strong.assign, 
                             j)
    
    before_BS_DISP_par = bs_disp_input_output(before_BS_DISP_par, 
                                              BS.DISP.input.1, 
                                              BS.DISP.input.2, 
                                              before_BS_DISP.output)
    
    BS_DISP_par = bs_disp_input_output(BS_DISP_par, 
                                       BS.DISP.input.1, 
                                       BS.DISP.input.2, 
                                       BS_DISP.output)
    
    ##### B3: output BS (bootstrap) & DISP files #####
    write.table(BS_par, 
                file = paste0(path.CF,"iniparams_BS_C_", i,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    write.table(DISP_par, 
                file = paste0(path.CF, "iniparams_DISP_C_", i,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    write.table(before_BS_DISP_par, 
                file = paste0(path.CF, "iniparams_BS_DISP_before_C_", i,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    write.table(BS_DISP_par, 
                file = paste0(path.CF, "iniparams_BS_DISP_C_", i,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
  }
}



###### Copy and paste the updated .csv file in cluster folders ######
# Specify the source directory path
noGUI_file <- "/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/PMF_NoGUI_NoCsub_NoExtreme_cluster" 

# Specify the base path for destination directories
noGUI_txt_folder <- "/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp/CSN_CMD_txt_noCsub_noExtreme"  

# Get a list of .csv file names of the conc & unc data for Non-GUI PMF
# Read those starting with CSN_ and end with _CMD.csv
cluster_file_list <- list.files(path = noGUI_file, pattern = "^CSN_.*_PMF_CMD\\.csv$")

# Loop through each .csv file name
for (file_name in cluster_file_list) {
  # Extract the cluster.No from the file name
  cluster.No = str_extract(file_name, "\\d+")
  
  # Name of Cluster folder
  cluster_folder = paste0("Cluster_", cluster.No, "/")
  
  # Construct the destination directory path
  cluster_dir <- file.path(noGUI_txt_folder, cluster_folder)
  
  # Create the destination directory if it doesn't exist
  # if (!dir.exists(cluster_dir)) {dir.create(cluster_dir)}
  
  # Full path to the source csv file
  src_cluster_path <- file.path(noGUI_file, file_name)
  
  # Copy the .csv file from the source directory to the destination directory
  file.copy(from = src_cluster_path, to = cluster_dir, recursive = T)
}





