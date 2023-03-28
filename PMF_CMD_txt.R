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


#### A. create cluster & sub-factor folders ####
## set path for all new folders
pathway = paste0(data.dir,"/CSN_CMD_txt/")

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

#### B. prepare iniparams .txt files for PMF via CMD ####
##### B01: read the original iniparams files #####
base_par = readLines("/Users/TingZhang/Downloads/PMF_no_GUI/iniparams_base_1.txt")
base_par[30]
class(base_par[30])
# np, line 30;
# main data file, line 37; base_par[37]
# output file, line 40; base_par[40]
DISP_par = readLines("/Users/TingZhang/Downloads/PMF_no_GUI/iniparams_DISP_2or3.txt")
BS_par = readLines("/Users/TingZhang/Downloads/PMF_no_GUI/iniparams_BS_2or3.txt")

##### B02: other info preparation #####
# read the information of all cluster
cluster_sum = read.csv("")

# common path for all output files
pathway = paste0(data.dir,"/CSN_CMD_txt/")

cluster.NO = 1:25
factor.number = c(6:11)

for(i in 1:length(cluster.NO)){
  ## info of selected cluster
  cluster_info = cluster_sum[cluster_sum$Final.decision == i]
  cluster_info = 
  
  cluster.row = cluster_info$row.NO[Final.decision == i]
  ## data of bad, weak, strong variables for selected cluster
  
  
  
##### B1: iniparams for Base run #####
  # create and replace the name of input file
  base.input = paste0("CSN_C_", cluster.NO[i], "_conc_unc.csv")
  base_par[37] = gsub("CSN_site_10730023_conc_unc_PMF_CMD.csv", 
                      base.input, 
                      base_par[37])
  
  # change n1 & n2, the number of rows and variables
  base_par[30] = gsub("293", 
                      cluster.row, 
                      base_par[30])
  base_par[30] = gsub("29", 
                      variable.NO, 
                      base_par[30])
  
  # replace the number of factors to use and name of output files
  for (j in 1:length(factor.number)){
    factor.no = factor.number[j]
    # output path
    path.CF = paste0(pathway, "/Cluster_", i, "/Factor_", (j+5), "/")
    
    base_par[30] = gsub("7", 
                        factor.no, 
                        base_par[30])
    base.output = paste0("CSN_C_", cluster.NO[i], "_F_", factor.no, "_")
    base_par[40] = gsub("CSN_site_10730023_", 
                        base.output, 
                        base_par[40])
    base.param.name = paste0("iniparams_base_CSN_C_", 
                             cluster.NO[i], "_F_", 
                             factor.no, ".txt")
    
    write.table(base_par, 
                file = paste0(path.CF, base.param.name), 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
  }



##### B2: iniparams for BS (bootstrap) run #####
  # create and replace the name of input file
  BS.input.1 = paste0("CSN_C_", cluster.NO[i], "_conc_unc.csv")
  BS_par[37] = gsub("CSN_site_10730023_conc_unc_PMF_CMD.csv", 
                    BS.input.1, 
                    BS_par[37])
  
  BS.input.2 = paste0("CSN_C_", cluster.NO[i], "_F_", factor.no, "_.dat")
  BS_par[37] = gsub("CSN_site_10730023_.dat", 
                    BS.input.2, 
                    BS_par[37])
  
  # change n1 & n2, the number of rows and variables
  BS_par[30] = gsub("293", 
                    cluster.row, 
                    BS_par[30])
  BS_par[30] = gsub("29", 
                    variable.NO, 
                    BS_par[30])
  
  # replace the number of factors to use and name of output files
  for (j in 1:length(factor.number)){
    factor.no = factor.number[j]
    BS_par[30] = gsub("7", 
                      factor.no, 
                      BS_par[30])
    BS.output = paste0("CSN_C_", cluster.NO[i], "_F_", factor.no, "_BS_")
    BS_par[40] = gsub("CSN_site_10730023_BS_", 
                      BS.output, 
                      BS_par[40])
    BS.param.name = paste0("iniparams_BS_CSN_C_", 
                           cluster.NO[i], "_F_", 
                           factor.no, ".txt")
    
    write.table(BS_par, 
                file = BS.param.name, 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
  }



  ##### B3.1: iniparams for DISP (Displacement) run #####
  # create and replace the name of input file
  DISP.input.1 = paste0("CSN_C_", cluster.NO[i], "_conc_unc.csv")
  DISP_par[37] = gsub("CSN_site_10730023_conc_unc_PMF_CMD.csv", 
                    DISP.input.1, 
                    DISP_par[37])
  
  DISP.input.2 = paste0("CSN_C_", cluster.NO[i], "_F_", factor.no, "_.dat")
  DISP_par[37] = gsub("CSN_site_10730023_.dat", 
                    DISP.input.2, 
                    DISP_par[37])
  
  # change n1 & n2, the number of rows and variables
  DISP_par[30] = gsub("293", 
                    cluster.row, 
                    DISP_par[30])
  DISP_par[30] = gsub("29", 
                    variable.NO, 
                    DISP_par[30])
  
  # replace the number of factors to use and name of output files
  for (j in 1:length(factor.number)){
    factor.no = factor.number[j]
    DISP_par[30] = gsub("7", 
                      factor.no, 
                      DISP_par[30])
    DISP.output = paste0("CSN_C_", cluster.NO[i], "_F_", factor.no, "_DISP_")
    DISP_par[40] = gsub("CSN_site_10730023__DISP_", 
                      DISP.output, 
                      DISP_par[40])

    ###### B3.2 iniparams - DISP: DISPBCMASK ######
    dispbcmask = "0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t1\t0\t1\t0\t1\t0"
    dispbcmask_use = rep(dispbcmask, factor.no)
    newContents <- c(contents[1:index], newLines, contents[(index+1):length(contents)])
    
    DISP_par_try[69] = dispbcmask_use
    
    DISP_par_try[69]
    DISP_par_try[64:74]
    DISP_par[69]
    DISP_par[64:74]
    
    
    DISP.param.name = paste0("iniparams_DISP_CSN_C_", 
                             cluster.NO[i], "_F_", 
                             factor.no, ".txt")
    
    write.table(DISP_par, 
                file = DISP.param.name, 
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
  }
}





Append=TRUE
filename='out.txt'
data1=data.frame(x=1:10)
data2=data.frame(y=1:3)
data3=data.frame(z=99)
write(c("\n\n","No of blues =", nrow(data1)), file=filename,append=FALSE)
write(c("\n\n","No of greens =", nrow(data2)), file=filename,append=Append)
write(c("\n\n","No of reds =", nrow(data3)), file=filename,append=Append)


