##clear environment
# rm(list=ls())

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp")
getwd()
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp"

##packages in need
library(tidyr) # separate{tidyr}, gather{tidyr}, spread{tidyr},  spread is VIP function, str_split_fixed{stringr} is better than separate
library(stats) # aggregate{stats}, VIP function
library(ggplot2)
library(scales) # percent{}
library(dplyr)

Base_report = read.table("/Users/TingZhang/Downloads/PMF_no_GUI copy/CSN_site_10730023_base.txt",
                         sep = "\t")

#### Result - Base Model ####
base.row = nrow(Base_report)
##### Base1: detect the task with the lowest Q#####
# generate the task-Q table
base_q = Base_report$V1[
  (base.row - 19): 
    base.row]
base_q = data.frame(base_q)
base_q_df <- base_q %>% 
  separate(base_q, 
           c("taskNum", "Qmain", "Qaux", "Q(XX)", "dQ<0", "factor", "correlations with Best-fit", "factors"),  
           sep = "\\s+")

##### Base2: Correlations of weighted F & G factors of task with the lowest Q#####
# pseudo factor.Num is 7 for now
# use F.correl.row[1] instead of F.correl.row for now, we may have records of multiple runs in preset txt

# correlations of F factors
F.correl.row = which(Base_report$V1 == 
                       "  Correlations of weighted F factors:")
F_correl = Base_report$V1[(
  F.correl.row[1]+1): 
    (F.correl.row[1]+factor.Num)]
F_correl = data.frame(F_correl)

F_correl_df <- F_correl %>% 
  separate(F_correl, 
           c("Blank", "FactorNum", 
             paste0(rep("Factor_", factor.Num), 
                    1:factor.Num)),  
           sep = "\\s+")
F_correl_df$Blank = NULL

# correlations of G factors
G.correl.row = which(Base_report$V1 == 
                       "  Correlations of G factors:")
G_correl = Base_report$V1[(G.correl.row[1]+1): 
                            (G.correl.row[1]+factor.Num)] 
G_correl = data.frame(G_correl)
G_correl_df <- G_correl %>% 
  separate(G_correl, 
           c("Blank", "FactorNum", 
             paste0(rep("Factor_", factor.Num), 
                    1:factor.Num)),  
           sep = "\\s+")
G_correl_df$Blank = NULL

##### Base3: time-series factor contribution of task with the lowest Q #####
matrix.AA.row = which(Base_report$V1 == 
                        "   Factor matrix AA   ")
matrix.BB.row = which(Base_report$V1 == 
                        "   Factor matrix BB   ")




