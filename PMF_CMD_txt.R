##clear environment
# rm(list=ls())

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp")
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

## function to create folders and sub-folders
create_dir = function (clusterNum, factorNum) {
  for (i in clusterNum){
    if(!dir.exists(
      paste0(
        pathway, i))){
      dir.create(
        file.path(
          paste0(
            pathway, i)),
        recursive = TRUE)
    }
    for (j in factorNum){
      if(!dir.exists(
        paste0(
          pathway, i, "/", j))){
        dir.create(
          file.path(
            paste0(
              pathway, i, "/", j)), 
          recursive = TRUE)
      }
    }
  }
}

## create new folders
create_dir(clusterNum, factorNum)


#### B start. read iniparams files  ####
base_par_org = readLines("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_no_GUI/iniparams_templates/iniparams_base_1.txt")
base_par_org[29]; base_par_org[30]
class(base_par_org[30])
# np, line 30;
# main data file, line 37; base_par_org[37]
# output file, line 40; base_par_org[40]
DISP_par_org = readLines("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_no_GUI/iniparams_templates/iniparams_DISP_3.txt")
BS_par_org = readLines("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_no_GUI/iniparams_templates/iniparams_BS_2.txt")
before_BS_DISP_par_org = readLines("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_no_GUI/iniparams_templates/iniparams_BS_PHASE_of_BSDISP_4.txt")
BS_DISP_par_org = readLines("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_no_GUI/iniparams_templates/iniparams_BS-DISP_5.txt")

# a random check
# base_par = readLines("/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp/CSN_CMD_txt_2023.05.15/Cluster_1/Factor_6/iniparams_base_C_1_F_6.txt")

cluster.NO = 1:25
factor.number = c(6:11)


#### B-1. SIMPLIFIED prepare iniparams .txt - All data, All Variables! ####
cluster_sum = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/PMF_NoGUI_cluster/CSN_PMF_CMD_StrongWeakBad_Cluster.csv")
# common path for all output files
pathway = paste0(data.dir,"/CSN_CMD_txt/")

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
    path.CF = paste0(pathway, "Cluster_", i, "/Factor_", (j), "/")
    
    # replace with the row, variable, & factor number 
    par_line_30 = gsub("\\b7\\b", j, 
                       gsub("\\b29\\b", variable.NO, 
                            gsub("\\b293\\b", cluster.row, 
                                 base_par[30])))
    
    base_par[30] = BS_par[30] = DISP_par[30] = 
      before_BS_DISP_par[30] = BS_DISP_par[30] = par_line_30
    
    ##### B1: iniparams for Base run #####
    # create and replace the names for base run input & output files
    base.input = paste0("CSN_C_", cluster.NO[i], "_PMF_CMD.csv")
    base.output = paste0("CSN_C_", cluster.NO[i], "_F_", j, "_")
    
    base_par[37] = gsub("CSN_site_10730023_conc_unc_PMF_CMD.csv", 
                        base.input, 
                        base_par[37])
    base_par[40] = gsub("CSN_site_10730023_", 
                        base.output, 
                        base_par[40])
    
    write.table(base_par, 
                file = paste0(path.CF, "iniparams_base_C_", i,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    ##### B2: iniparams common input for for BS (bootstrap) & DISP file #####
    # create and replace the name of input file
    BS.DISP.input.1 = paste0("CSN_C_", cluster.NO[i], "_PMF_CMD.csv")
    BS.DISP.input.2 = paste0("CSN_C_", cluster.NO[i], "_F_", j, "_.dat")
    
    bs_disp_line_37 = gsub("CSN_site_10730023_conc_unc_PMF_CMD.csv", 
                           BS.DISP.input.1, 
                           gsub("CSN_site_10730023_.dat",
                                BS.DISP.input.2, 
                                BS_par[37]))
    
    BS_par[37] = DISP_par[37] = 
      before_BS_DISP_par[37] = BS_DISP_par[37] = 
      bs_disp_line_37
    
    ##### B3: iniparams common input for for BS (bootstrap) #####
    BS.output = paste0("CSN_C_", cluster.NO[i], "_F_", j, "_BS_")
    BS_par[40] = gsub("CSN_site_10730023_BS_", 
                      BS.output, 
                      BS_par[40])
    
    write.table(BS_par, 
                file = paste0(path.CF,"iniparams_BS_C_", i,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    ##### B4: iniparams for DISP (Displacement) #####
    DISP.output = paste0("CSN_C_", cluster.NO[i], "_F_", j, "_DISP_")
    DISP_par[40] = gsub("CSN_site_10730023__DISP_", 
                        DISP.output, 
                        DISP_par[40])
    
    ## DISP: DISPBCMASK
    # dispbcmask = "0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t1\t0\t1\t0\t1\t0"
    dispbcmask = rep(wead.strong.assign, j)
    
    # Replace the Strong, Weak setting line 69 with N.factor copies of itself
    DISP_par <- c(DISP_par[1:68], 
                  dispbcmask, 
                  DISP_par[(70):length(DISP_par)])
    
    write.table(DISP_par, 
                file = paste0(path.CF, "iniparams_DISP_C_", i,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    ##### B5: iniparams for BS-DISP #####
    
    BS_DISP.output = paste0("CSN_C_", cluster.NO[i], "_F_", j, "_BS_DISP_")
    BS_DISP_par[40] = gsub("CSN_site_10730023_BS-DISP_", 
                           BS_DISP.output, 
                           BS_DISP_par[40])

    before_BS_DISP.output = paste0("CSN_C_", cluster.NO[i], "_F_", j, "_before_BS_DISP_")
    before_BS_DISP_par[40] = gsub("CSN_site_10730023_BS_", 
                                  before_BS_DISP.output, 
                                  before_BS_DISP_par[40])
    
    ###### BS_DISP: DISPBCMASK
    BS_DISP_par <- c(BS_DISP_par[1:68], 
                     dispbcmask, 
                     BS_DISP_par[(70):length(BS_DISP_par)])
    
    
    ## output files
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


#### B-2. SIMPLIFIED prepare iniparams .txt - All data, NO OC/EC Subgroups! ####
cluster_sum = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/PMF_NoGUI_NoCsub_cluster/CSN_noCsub_PMF_CMD_StrongWeakBad_Cluster.csv")
# common path for all output files
pathway = paste0(data.dir,"/CSN_CMD_txt_noCsub/")

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
    path.CF = paste0(pathway, "Cluster_", i, "/Factor_", (j), "/")
    
    # replace with the row, variable, & factor number 
    par_line_30 = gsub("\\b7\\b", j, 
                       gsub("\\b29\\b", variable.NO, 
                            gsub("\\b293\\b", cluster.row, 
                                 base_par[30])))
    
    base_par[30] = BS_par[30] = DISP_par[30] = 
      before_BS_DISP_par[30] = BS_DISP_par[30] = par_line_30
    
    ##### B1: iniparams for Base run #####
    
    # create and replace the names for base run input & output files
    base.input = paste0("CSN_noCsub_C_", cluster.NO[i], "_PMF_CMD.csv")
    base.output = paste0("CSN_noCsub_C_", cluster.NO[i], "_F_", j, "_")
    
    base_par[37] = gsub("CSN_site_10730023_conc_unc_PMF_CMD.csv", 
                        base.input, 
                        base_par[37])
    base_par[40] = gsub("CSN_site_10730023_", 
                        base.output, 
                        base_par[40])
    
    write.table(base_par, 
                file = paste0(path.CF, "iniparams_base_C_", i,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    ##### B2: iniparams common input for for BS (bootstrap) & DISP file #####
    # create and replace the name of input file
    BS.DISP.input.1 = paste0("CSN_noCsub_C_", cluster.NO[i], "_PMF_CMD.csv")
    BS.DISP.input.2 = paste0("CSN_noCsub_C_", cluster.NO[i], "_F_", j, "_.dat")
    
    bs_disp_line_37 = gsub("CSN_site_10730023_conc_unc_PMF_CMD.csv", 
                           BS.DISP.input.1, 
                           gsub("CSN_site_10730023_.dat",
                                BS.DISP.input.2, 
                                BS_par[37]))
    
    BS_par[37] = DISP_par[37] = 
      before_BS_DISP_par[37] = BS_DISP_par[37] = 
      bs_disp_line_37
    
    ##### B3: iniparams common input for for BS (bootstrap) #####
    BS.output = paste0("CSN_noCsub_C_", cluster.NO[i], "_F_", j, "_BS_")
    BS_par[40] = gsub("CSN_site_10730023_BS_", 
                      BS.output, 
                      BS_par[40])
    
    write.table(BS_par, 
                file = paste0(path.CF,"iniparams_BS_C_", i,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    ##### B4: iniparams for DISP (Displacement) #####
    DISP.output = paste0("CSN_noCsub_C_", cluster.NO[i], "_F_", j, "_DISP_")
    DISP_par[40] = gsub("CSN_site_10730023__DISP_", 
                        DISP.output, 
                        DISP_par[40])
    
    ## DISP: DISPBCMASK
    # dispbcmask = "0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t1\t0\t1\t0\t1\t0"
    dispbcmask = rep(wead.strong.assign, j)
    
    # Replace the Strong, Weak setting line 69 with N.factor copies of itself
    DISP_par <- c(DISP_par[1:68], 
                  dispbcmask, 
                  DISP_par[(70):length(DISP_par)])
    
    write.table(DISP_par, 
                file = paste0(path.CF, "iniparams_DISP_C_", i,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    ##### B5: iniparams for BS-DISP #####
    
    BS_DISP.output = paste0("CSN_noCsub_C_", cluster.NO[i], "_F_", j, "_BS_DISP_")
    BS_DISP_par[40] = gsub("CSN_site_10730023_BS-DISP_", 
                           BS_DISP.output, 
                           BS_DISP_par[40])
    
    before_BS_DISP.output = paste0("CSN_noCsub_C_", cluster.NO[i], "_F_", j, "_before_BS_DISP_")
    before_BS_DISP_par[40] = gsub("CSN_site_10730023_BS_", 
                                  before_BS_DISP.output, 
                                  before_BS_DISP_par[40])
    
    ###### BS_DISP: DISPBCMASK
    BS_DISP_par <- c(BS_DISP_par[1:68], 
                     dispbcmask, 
                     BS_DISP_par[(70):length(BS_DISP_par)])
    
    
    ## output files
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


#### B-3. SIMPLIFIED prepare iniparams .txt  - No extreme values, NO OC/EC Subgroups! ####
cluster_sum = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/PMF_NoGUI_NoCsub_NoExtreme_cluster/CSN_noCsub_noExtreme_PMF_CMD_StrongWeakBad_Cluster.csv")
# common path for all output files
pathway = paste0(data.dir,"/CSN_CMD_txt_noCsub_noExtreme/")

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
    path.CF = paste0(pathway, "Cluster_", i, "/Factor_", (j), "/")
    
    # replace with the row, variable, & factor number 
    par_line_30 = gsub("\\b7\\b", j, 
                       gsub("\\b29\\b", variable.NO, 
                            gsub("\\b293\\b", cluster.row, 
                                 base_par[30])))
    
    base_par[30] = BS_par[30] = DISP_par[30] = 
      before_BS_DISP_par[30] = BS_DISP_par[30] = par_line_30
    
    ##### B1: iniparams for Base run #####
    
    # create and replace the names for base run input & output files
    base.input = paste0("CSN_noCsub_noExtreme_C_", cluster.NO[i], "_PMF_CMD.csv")
    base.output = paste0("CSN_noCsub_noExtreme_C_", cluster.NO[i], "_F_", j, "_")
    
    base_par[37] = gsub("CSN_site_10730023_conc_unc_PMF_CMD.csv", 
                        base.input, 
                        base_par[37])
    base_par[40] = gsub("CSN_site_10730023_", 
                        base.output, 
                        base_par[40])
    
    write.table(base_par, 
                file = paste0(path.CF, "iniparams_base_C_", i,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    ##### B2: iniparams common input for for BS (bootstrap) & DISP file #####
    # create and replace the name of input file
    BS.DISP.input.1 = paste0("CSN_noCsub_noExtreme_C_", cluster.NO[i], "_PMF_CMD.csv")
    BS.DISP.input.2 = paste0("CSN_noCsub_noExtreme_C_", cluster.NO[i], "_F_", j, "_.dat")
    
    bs_disp_line_37 = gsub("CSN_site_10730023_conc_unc_PMF_CMD.csv", 
                           BS.DISP.input.1, 
                           gsub("CSN_site_10730023_.dat",
                                BS.DISP.input.2, 
                                BS_par[37]))
    
    BS_par[37] = DISP_par[37] = 
      before_BS_DISP_par[37] = BS_DISP_par[37] = 
      bs_disp_line_37
    
    ##### B3: iniparams common input for for BS (bootstrap) #####
    BS.output = paste0("CSN_noCsub_noExtreme_C_", cluster.NO[i], "_F_", j, "_BS_")
    BS_par[40] = gsub("CSN_site_10730023_BS_", 
                      BS.output, 
                      BS_par[40])
    
    write.table(BS_par, 
                file = paste0(path.CF,"iniparams_BS_C_", i,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    ##### B4: iniparams for DISP (Displacement) #####
    DISP.output = paste0("CSN_noCsub_noExtreme_C_", cluster.NO[i], "_F_", j, "_DISP_")
    DISP_par[40] = gsub("CSN_site_10730023__DISP_", 
                        DISP.output, 
                        DISP_par[40])
    
    ## DISP: DISPBCMASK
    # dispbcmask = "0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t1\t0\t1\t0\t1\t0"
    dispbcmask = rep(wead.strong.assign, j)
    
    # Replace the Strong, Weak setting line 69 with N.factor copies of itself
    DISP_par <- c(DISP_par[1:68], 
                  dispbcmask, 
                  DISP_par[(70):length(DISP_par)])
    
    write.table(DISP_par, 
                file = paste0(path.CF, "iniparams_DISP_C_", i,
                              "_F_", j, ".txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    ##### B5: iniparams for BS-DISP #####
    
    BS_DISP.output = paste0("CSN_noCsub_noExtreme_C_", cluster.NO[i], "_F_", j, "_BS_DISP_")
    BS_DISP_par[40] = gsub("CSN_site_10730023_BS-DISP_", 
                           BS_DISP.output, 
                           BS_DISP_par[40])
    
    before_BS_DISP.output = paste0("CSN_noCsub_noExtreme_C_", cluster.NO[i], "_F_", j, "_before_BS_DISP_")
    before_BS_DISP_par[40] = gsub("CSN_site_10730023_BS_", 
                                  before_BS_DISP.output, 
                                  before_BS_DISP_par[40])
    
    ###### BS_DISP: DISPBCMASK
    BS_DISP_par <- c(BS_DISP_par[1:68], 
                     dispbcmask, 
                     BS_DISP_par[(70):length(BS_DISP_par)])
    
    
    ## output files
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


#### examples of matching pattern definition, and position determination ####

# For matching a "0" only when it's the last character on the line:
pattern <- "0$"

# For matching a standalone "0" that's not part of another number:
pattern <- "\\b0\\b"

replacement <- "20" # The value you want to replace the pattern with

# Loop through the lines
for (i in seq_along(lines)) {
  # Find the position of the pattern in the current line
  position_to_replace <- regexpr(pattern, lines[i]) 
  
  # If the pattern is found (regexpr returns a positive value if found)
  if (position_to_replace > 0) {
    # Replace the pattern at the identified position with the replacement
    lines[i] <- substr_replace(lines[i], replacement, position_to_replace, position_to_replace + nchar(pattern) - 1)
  }
}


#### B. Early Version - prepare iniparams .txt files for PMF via CMD (till 2023.04) ####
##### B01: read the original iniparams files #####
base_par = readLines("/Users/TingZhang/Downloads/PMF_no_GUI/iniparams_base_1.txt")
base_par[29]; base_par[30]
class(base_par[30])
# np, line 30;
# main data file, line 37; base_par[37]
# output file, line 40; base_par[40]
DISP_par = readLines("/Users/TingZhang/Downloads/PMF_no_GUI/iniparams_DISP_3.txt")
BS_par = readLines("/Users/TingZhang/Downloads/PMF_no_GUI/iniparams_BS_2.txt")
before_BS_DISP_par = readLines("/Users/TingZhang/Downloads/PMF_no_GUI/iniparams_BS_PHASE_of_BSDISP_4.txt")
BS_DISP_par = readLines("/Users/TingZhang/Downloads/PMF_no_GUI/iniparams_BS-DISP_5.txt")

##### B02: other info preparation #####
# read the information of all cluster
cluster_sum = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/PMF_NoGUI_cluster/CSN_PMF_CMD_StrongWeakBad_Cluster.csv")
cluster_sum$X = NULL

# common path for all output files
pathway = paste0(data.dir,"/CSN_CMD_txt/")

cluster.NO = 1:25
factor.number = c(6:11)

for(i in cluster.NO){
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
  
##### B1: iniparams for Base run #####
  # create and replace the name of input file
  base.input = paste0("CSN_C_", cluster.NO[i], "_PMF_CMD.csv")
  base_par[37] = gsub("CSN_site_10730023_conc_unc_PMF_CMD.csv", 
                      base.input, 
                      base_par[37])
  
  # replace the number of factors to use and name of output files
  for (j in factor.number){
    # output path
    path.CF = paste0(pathway, "Cluster_", i, "/Factor_", (j), "/")
    
    # replace with the row, variable, & factor number 
    base_par[30] = gsub("\\b9\\b", j, 
                        gsub("\\b29\\b", variable.NO, 
                             gsub("\\b293\\b", cluster.row, 
                                  base_par[30])))
    
    base.output = paste0("CSN_C_", cluster.NO[i], "_F_", j, "_")
    base_par[40] = gsub("CSN_site_10730023_", 
                        base.output, 
                        base_par[40])

    write.table(base_par, 
                file = paste0(path.CF, "iniparams_base.txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
  }



##### B2: iniparams for BS (bootstrap) run #####
  # create and replace the name of input file
  BS.input.1 = paste0("CSN_C_", cluster.NO[i], "_PMF_CMD.csv")
  BS.input.2 = paste0("CSN_C_", cluster.NO[i], "_F_", j, "_.dat")

  BS_par[37] = gsub("CSN_site_10730023_conc_unc_PMF_CMD.csv", 
                      BS.input.1, 
                      gsub("CSN_site_10730023_.dat",
                           BS.input.2, 
                           BS_par[37]))
  
  
  # replace the number of factors to use and name of output files
  for (j in factor.number){
    path.CF = paste0(pathway, "Cluster_", i, "/Factor_", j, "/")
    
    BS_par[30] = gsub("\\b9\\b", j, 
                      gsub("\\b29\\b", variable.NO, 
                           gsub("\\b293\\b", cluster.row, 
                                BS_par[30])))

    BS.output = paste0("CSN_C_", cluster.NO[i], "_F_", j, "_BS_")
    BS_par[40] = gsub("CSN_site_10730023_BS_", 
                      BS.output, 
                      BS_par[40])

    write.table(BS_par, 
                file = paste0(path.CF,"iniparams_BS.txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
  }



  ##### B3.1: iniparams for DISP (Displacement) run #####
  # create and replace the name of input file
  DISP.input.1 = paste0("CSN_C_", cluster.NO[i], "_PMF_CMD.csv")
  DISP.input.2 = paste0("CSN_C_", cluster.NO[i], "_F_", j, "_.dat")
  
  DISP_par[37] = gsub("CSN_site_10730023_conc_unc_PMF_CMD.csv", 
                    DISP.input.1, 
                    gsub("CSN_site_10730023_.dat",
                         DISP.input.2, 
                         DISP_par[37]))
  
  # replace the number of factors to use and name of output files
  for (j in factor.number){
    path.CF = paste0(pathway, "Cluster_", i, "/Factor_", j, "/")
    
    DISP_par[30] = gsub("\\b9\\b", j, 
                        gsub("\\b29\\b", variable.NO, 
                             gsub("\\b293\\b", cluster.row, 
                                  DISP_par[30])))

    DISP.output = paste0("CSN_C_", cluster.NO[i], "_F_", j, "_DISP_")
    DISP_par[40] = gsub("CSN_site_10730023__DISP_", 
                      DISP.output, 
                      DISP_par[40])

    ###### B3.2 iniparams - DISP: DISPBCMASK ######
    dispbcmask = "0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t1\t0\t1\t0\t1\t0"
    dispbcmask_use = rep(dispbcmask, j)
    newContents <- c(contents[1:index], newLines, contents[(index+1):length(contents)])
    
    DISP_par_try[69] = dispbcmask_use
    
    DISP_par[69]
    DISP_par[64:74]
    DISP_par[69]
    DISP_par[64:74]
    
    
    write.table(DISP_par, 
                file = paste0(path.CF, "iniparams_DISP.txt"), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
  }
    
    ##### B4.1: iniparams for BS-DISP run #####
    # create and replace the name of input file
    BS_DISP.input.1 = paste0("CSN_C_", cluster.NO[i], "_PMF_CMD.csv")
    BS_DISP_par[37] = gsub("CSN_site_10730023_conc_unc_PMF_CMD.csv", 
                        BS_DISP.input.1, 
                        BS_DISP_par[37])
    
    BS_DISP.input.2 = paste0("CSN_C_", cluster.NO[i], "_F_", j, "_.dat")
    BS_DISP_par[37] = gsub("CSN_site_10730023_.dat", 
                        BS_DISP.input.2, 
                        BS_DISP_par[37])
    
    # replace the number of factors to use and name of output files
    for (j in factor.number){
      path.CF = paste0(pathway, "Cluster_", i, "/Factor_", j, "/")
      
      BS_DISP_par[30] = gsub("\\b9\\b", j, 
                          gsub("\\b29\\b", variable.NO, 
                               gsub("\\b293\\b", cluster.row, 
                                    BS_DISP_par[30])))
      
      BS_DISP.output = paste0("CSN_C_", cluster.NO[i], "_F_", j, "_BS_DISP_")
      BS_DISP_par[40] = gsub("CSN_site_10730023__BS_DISP_", 
                          BS_DISP.output, 
                          BS_DISP_par[40])
      
      ###### B4.2 iniparams - BS_DISP: BS_DISPBCMASK ######
      BS_DISPbcmask = "0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t1\t0\t1\t0\t1\t0"
      BS_DISPbcmask_use = rep(BS_DISPbcmask, j)
      #newContents <- c(contents[1:index], newLines, contents[(index+1):length(contents)])
      
      BS_DISP_par_try[69] = BS_DISPbcmask_use
      
      BS_DISP_par_try[69]
      BS_DISP_par_try[64:74]
      BS_DISP_par[69]
      BS_DISP_par[64:74]
      
      
      write.table(BS_DISP_par, 
                  file = paste0(path.CF, "iniparams_BS_DISP.txt"), 
                  sep = "\t",
                  quote = FALSE,
                  row.names = FALSE,
                  col.names = FALSE)
  }
}


###### Copy and paste the updated .csv file in cluster folders ######
# Specify the source directory path
noGUI_file <- "/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/PMF_NoGUI_NoCsub_NoExtreme_cluster" 

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





