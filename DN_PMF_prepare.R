##clear environment
# rm(list=ls())

##set working directory
setwd("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE")
getwd()
data.dir <- "/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE"

library(tidyverse)
library(readxl)
library(data.table)
library(dplyr)
library(plyr)
library(ggplot2) 
library(base)
library(ggrepel)
library(bspec) # signal-to-noise ratio, snr{}
##### import and match CSN #####

# import CSN
csn_daily_before = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_interpulation_random-forest_afterLog_before_2015.csv")
csn_daily_after = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_interpulation_random-forest_afterLog_after_2015.csv")
csn_daily_before$X = csn_daily_after$X = NULL
csn_daily_before$Date = as.Date(csn_daily_before$Date)
csn_daily_after$Date = as.Date(csn_daily_after$Date)

# variables not to be used for PMF
# since 2023-02-16, add "NH4+" into species file for PMF analysis
csn_bfr_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC.unadjusted.88", "EC.unadjusted.88", "OPC.unadjusted.88",
                   "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
                   "K.", "Na.", "Sb", "Sn", 
                   "Rb", "Zr")

csn_aft_remove = c("EC1.unadjusted.88", "EC2.unadjusted.88", "EC3.unadjusted.88",  
                   "OC1.unadjusted.88", "OC2.unadjusted.88", "OC3.unadjusted.88", "OC4.unadjusted.88",
                   "OC1.88", "OC2.88", "OC3.88", "OC4.88",
                   "EC1.88", "EC2.88", "EC3.88", 
                   "EC1", "EC2", "EC3", 
                   "EC.unadjusted.88", "EC.TOR.unadjust.88", "EC.88",
                   "OC.unadjusted.88", "OC.TOR.unadjusted.88", "OC", 
                   "OPC.88", "OP.TOR.unadjusted.88", "OPC.unadjusted.88",
                   "Ag", "Ba", "Cd", "Ce", "Co", "Cs", "In", 
                   "K.", "Na.", "Sb", "Sn", 
                   "Rb", "Zr", "Cl.")

csn_daily_before[ ,csn_bfr_remove] <- list(NULL)
csn_daily_after[ ,csn_aft_remove] <- list(NULL)

# reset colnames
colnames(csn_daily_before)[c(5, 13, 22, 23)] = c("PM25", "EC", "OC", "OP")
colnames(csn_daily_after)[c(12, 21, 22, 25)] = c("EC", "OC", "OP", "PM25")

# move PM2.5 concentration to the last column
csn_daily_before <- 
  csn_daily_before %>% 
  relocate(PM25, .after = last_col())
csn_daily_after <- 
  csn_daily_after %>% 
  relocate(PM25, .after = last_col())

setDT(csn_daily_before)
setDT(csn_daily_after)

csn_daily = rbind(csn_daily_before, csn_daily_after)

#### PM2.5 concentration distribution - Sites & Clusters ####
# csn_daily$year = year(csn_daily$Date)
csn_cluster = read.csv("CSN_RF_cluster5training.csv")

csn_PM_avg = ddply(csn_daily, 
                   .(SiteCode), 
                   summarise,
                   PM25 = mean(PM25))

csn_PM_avg = merge(csn_PM_avg, csn_cluster)

# the mean PM level of each cluster
csn_PM_cluster = ddply(csn_PM_avg, 
                   .(Final.Decision), 
                   summarise,
                   PM25 = mean(PM25))


# cluster with the highest and lowest PM
cluster.top.PM = csn_PM_cluster$Final.Decision[
  which(
    csn_PM_cluster$PM25 == 
      max(csn_PM_cluster$PM25))]
cluster.least.PM = csn_PM_cluster$Final.Decision[
  which(
    csn_PM_cluster$PM25 == 
      min(csn_PM_cluster$PM25))]

# sites in clusters with the highest and lowest PM
sites.top.PM.cluster = csn_PM_avg$SiteCode[
  which(
    csn_PM_avg$Final.Decision == 
      cluster.top.PM)]
sites.least.PM.cluster = csn_PM_avg$SiteCode[
  which(
    csn_PM_avg$Final.Decision == 
      cluster.least.PM)]

# number of sites in each cluster
csn_each_cluster = data.frame(table(csn_cluster$Final.Decision))
colnames(csn_each_cluster) = c("Final.Decision", "No.Site.in.Cluster")

# get the GPS of sites
cty_rural_urban = read.csv("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$X = cty_rural_urban$X.1 = NULL

site_gps = select(cty_rural_urban, SiteCode, Longitude, Latitude)

csn_PM_site = merge(csn_PM_avg, site_gps)
csn_PM_site = merge(csn_PM_site, csn_cluster)

csn_PM_site$PM.level = "Middle"
csn_PM_site$PM.level[
  csn_PM_site$SiteCode %in% 
    sites.top.PM.cluster] = "Highest.PM"
csn_PM_site$PM.level[
  csn_PM_site$SiteCode %in% 
    sites.least.PM.cluster] = "Lowest.PM"

sapply(csn_PM_site, class)

UScounty <- map_data("county")
# map site-average concentration
ggplot(csn_PM_site, 
       aes(Longitude, Latitude, color = PM25, group = PM.level)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.8, aes(shape = PM.level)) +
  scale_color_continuous(low = "steelblue", high = "darkorange2") +
  theme_bw()


#### Selection1: use the MDL based on CSN dataset ####
###### CSN_MDL - prepare site & date matched MDL ###### 
# csn_daily_conc = csn_daily
# csn_daily_conc = csn_daily_cluster_top_PM
csn_daily_conc$State = csn_daily_conc$Qualifier = NULL
csn_daily_conc$year = year(csn_daily_conc$Date)
csn_daily_conc$month = month(csn_daily_conc$Date)

# get monthly MDL
csn_mdl = read.csv("/Users/ztttttt/Documents/HEI PMF/R - original IMPROVE/CSN_MDL_monthly.csv")
csn_mdl$X = NULL

# reorder the dataset for matching
csn_daily_conc = csn_daily_conc[with(
  csn_daily_conc, 
  order(SiteCode, year, month)), ]
csn_mdl = csn_mdl[with(
  csn_mdl, 
  order(SiteCode, year, month)), ]

# check if columns from concentration & MDL datasets match
summary(colnames(csn_daily_conc)[3:(ncol(csn_daily_conc)-2)] == 
          colnames(csn_mdl)[4:ncol(csn_mdl)])
dim(csn_daily_conc)
dim(csn_mdl)

# calculated the monthly MDL for OC, EC & OPC
## 1. NO MDL data for them
## 2. MDL for given subgroups 20 changed, like OC2.88 & OC2, 0.24 vs. 0.017 
## use the 10th percentile value
csn_mdl_OCEC = ddply(csn_daily_conc, .(SiteCode, year, month),
                     summarise,
                     EC.10th = quantile(EC, 0.1, na.rm = T),
                     OC.10th = quantile(OC, 0.1, na.rm = T),
                     OP.10th = quantile(OP, 0.1, na.rm = T))

# replace and remove the old value, which are all -999
csn_mdl_newOCEC = merge(csn_mdl, csn_mdl_OCEC)
csn_mdl_newOCEC$EC = csn_mdl_newOCEC$EC.10th
csn_mdl_newOCEC$OC = csn_mdl_newOCEC$OC.10th
csn_mdl_newOCEC$OP = csn_mdl_newOCEC$OP.10th
csn_mdl_newOCEC$EC.10th = csn_mdl_newOCEC$OC.10th = csn_mdl_newOCEC$OP.10th = NULL
summary(csn_mdl_newOCEC)

# expand MDL file to daily measurement 
# (in case of interpolation, not used original data directly)
csn_daily_conc_date = select(csn_daily_conc, SiteCode, Date, year, month)
csn_daily_fullMDL = merge(csn_daily_conc_date, csn_mdl_newOCEC, all.x = T)
dim(csn_daily_conc_date)

# move Date to the first column
csn_daily_fullMDL =
  csn_daily_fullMDL %>%
  relocate(Date, .before = SiteCode)

# reorder the dataset for matching
csn_daily_fullMDL = csn_daily_fullMDL[with(
  csn_daily_fullMDL, 
  order(SiteCode, year, month)), ]

csn_daily_fullMDL$year = csn_daily_fullMDL$month = 
  csn_daily_conc$year = csn_daily_conc$month = NULL

# Again check if columns from concentration & new MDL datasets match
summary(colnames(csn_daily_conc) == 
          colnames(csn_daily_fullMDL))

###### CSN_MDL - estimate the uncertainty ###### 
# compare concentration and MDL of a given component
csn_conc = csn_daily_conc[, 3:ncol(csn_daily_conc)]
csn_mdl = csn_daily_fullMDL[, 3:ncol(csn_daily_fullMDL)]
csn_conc_mdl = data.frame(Map(">", csn_conc, csn_mdl))
setDT(csn_conc_mdl)

# check variable class & dataset dimenssion
sapply(csn_conc, class)
sapply(csn_mdl, class)
sapply(csn_conc_mdl, class)
dim(csn_conc)
dim(csn_mdl)
dim(csn_conc_mdl)

# test uncertainty, error_fraction 
comp_error_fraction = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/CSN_k_Error Fraction.csv")
summary(colnames(comp_error_fraction)[2:ncol(comp_error_fraction)] == colnames(csn_conc_mdl))
comp_error_fraction$data = NULL

# expand error_fraction file to one with same row number as concentration file
comp_ef = do.call("rbind", replicate(nrow(csn_conc_mdl), 
                                     comp_error_fraction[1, ], 
                                     simplify = F))
setDT(comp_ef)

# assign data conditionally to concentration and uncertainty values for PMF
conc_pmf = csn_conc_mdl * csn_conc +
  (!csn_conc_mdl) * csn_mdl * 0.5
unc_pmf = csn_conc_mdl * (csn_mdl / 3 + comp_ef * csn_conc) +
  (!csn_conc_mdl) * 5/6 * csn_mdl
unc_pmf$PM25 = 3 * csn_daily_conc$PM25

# insert date into  concentration & uncertainty datasets
conc_pmf <- cbind(csn_daily[, 1:4], conc_pmf)
unc_pmf <- cbind(csn_daily[, 1:4], unc_pmf)

csn_conc_mdl_Site = cbind(csn_daily[, 1:4], csn_conc_mdl)

write.csv(conc_pmf, "CSN_concentration_for_PMF.csv")
write.csv(unc_pmf, "CSN_uncertainty_for_PMF.csv")
write.csv(csn_conc_mdl_Site, "CSN_concentration_vs_MDL.csv")

###### CSN_MDL - daily PM of selected cluster - PMF priori try ###### 
# determine the cluster with highest average PM concentrations
csn_PM_cluster = merge(csn_PM_cluster, csn_each_cluster)
Cluster.top.PM = csn_PM_cluster$Final.Decision[
  which(csn_PM_cluster$PM25 == 
          max(csn_PM_cluster$PM25))]
# cluster 6, with five sites

# map the sites with the highest PM average
ggplot(subset(csn_PM_site, Final.Decision == Cluster.top.PM), 
       aes(Longitude, Latitude, color= PM25)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6, shape = 16) 

# extract concentration & uncertainty of selected sites for PMF
csn_select_conc = subset(conc_pmf, SiteCode %in% 
                                    csn_cluster$SiteCode[which(
                                      csn_cluster$Final.Decision == Cluster.top.PM)])
csn_select_unc = subset(unc_pmf, SiteCode %in% 
                           csn_cluster$SiteCode[which(
                             csn_cluster$Final.Decision == Cluster.top.PM)])

csn_select_conc_use = subset(csn_select_conc, Date < as.Date("2014-01-01"))
csn_select_unc_use = subset(csn_select_unc, Date < as.Date("2014-01-01"))
csn_select_site_row = data.frame(table(csn_select_conc_use$SiteCode))
colnames(csn_select_site_row)[1] = c("SiteCode")

csn_select_conc_1site = subset(csn_select_conc_use, 
                                      SiteCode == csn_select_site_row$SiteCode[3])
csn_select_unc_1site = subset(csn_select_unc_use, 
                                     SiteCode == csn_select_site_row$SiteCode[3])

csn_select_conc_use$SiteCode = csn_select_unc_use$SiteCode = 
  csn_select_conc_1site$SiteCode = csn_select_unc_1site$SiteCode = NULL

write.csv(csn_select_conc_use, "CSN_Cluster6_2011-14_PMF_conc.csv")
write.csv(csn_select_unc_use, "CSN_Cluster6_2011-14_PMF_unc.csv")
write.csv(csn_select_site_row, "CSN_Cluster6_date_number_of_site.csv")

write.csv(csn_select_conc_1site, "CSN_Cluster6_1site_2011-14_PMF_conc.csv")
write.csv(csn_select_unc_1site, "CSN_Cluster6_1site_2011-14_PMF_unc.csv")

# the result for the randomly selected site "60371103" is not good
## checking original concentrations
csn_daily_conc_cluster6_1site = subset(csn_daily,
                                       Date < as.Date("2014-01-01") &
                                         SiteCode == csn_select_site_row$SiteCode[3])
summary(csn_daily_conc_cluster6_1site)
sapply(csn_daily_conc_cluster6_1site, max)

### check the distribution high value of given component
conc_cluster6_1site_highK = subset(csn_daily_conc_cluster6_1site, 
                                   K > 0.2)

### check the influence of qualifier 
conc_cluster6_1site_qlf5 = subset(csn_daily_conc_cluster6_1site, 
                                  grepl(5, Qualifier, fixed = T))
summary(conc_cluster6_1site_qlf5)
sapply(conc_cluster6_1site_qlf5, max)

## checking the ratio of concentrations below MDL for PMF resetting
csn_conc_mdl_Site_cluster6_1site = subset(csn_conc_mdl_Site,
                                          Date < as.Date("2014-01-01") &
                                            SiteCode == csn_select_site_row$SiteCode[3])
summary(csn_conc_mdl_Site_cluster6_1site)
round(sapply(
  csn_conc_mdl_Site_cluster6_1site[, 5:ncol(csn_conc_mdl_Site_cluster6_1site)], 
  sum)/356*100, 
  0)

#### Selection2: use the MDL from EPA, one value for all period ####
###### EPA_MDL - prepare site & date matched MDL ###### 
csn_daily_conc = csn_daily
csn_daily_conc$State = csn_daily_conc$Qualifier = NULL
csn_daily_conc$year = year(csn_daily_conc$Date)
csn_daily_conc$month = month(csn_daily_conc$Date)

# reorder the dataset for matching
csn_daily_conc = csn_daily_conc[with(
  csn_daily_conc, 
  order(SiteCode, year, month)), ]

# move Date to the first column
csn_daily_conc =
  csn_daily_conc %>%
  relocate(Date, .before = SiteCode)

csn_daily_conc$year = csn_daily_conc$month = NULL

# get EPA MDL
csn_mdl_epa = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/CSN_EPA_BZ_MDL.csv")
csn_mdl_epa$X = csn_mdl_epa$data = NULL

# expand MDL file to daily measurement 
# previous slow method: csn_mdl = do.call("rbind", replicate(nrow(csn_daily_conc),csn_mdl_epa[1, ], simplify = F))
csn_mdl = csn_mdl_epa[rep(1, each = nrow(csn_daily_conc)), ]
setDT(csn_mdl)

# Again check if columns from concentration & new MDL datasets match
summary(colnames(csn_daily_conc)[3:ncol(csn_daily_conc)] == 
          colnames(csn_mdl))

###### EPA_MDL - estimate the uncertainty ###### 
# compare concentration and MDL of a given component
csn_conc = csn_daily_conc[, 3:ncol(csn_daily_conc)]
csn_conc_mdl = data.frame(Map(">", csn_conc, csn_mdl))
setDT(csn_conc_mdl)

# check variable class & dataset dimenssion
sapply(csn_conc, class)
sapply(csn_mdl, class)
sapply(csn_conc_mdl, class)
dim(csn_conc)
dim(csn_mdl)
dim(csn_conc_mdl)

# test uncertainty, error_fraction 
comp_error_fraction = read.csv("/Users/ztttttt/Documents/HEI PMF/IMPROVE & CSN original/CSN_k_Error Fraction.csv")
summary(colnames(comp_error_fraction)[2:ncol(comp_error_fraction)] == colnames(csn_conc_mdl))
comp_error_fraction$data = NULL

# expand error_fraction file to one with same row number as concentration file
# previous slow method: comp_ef = do.call("rbind", replicate(nrow(csn_conc_mdl), comp_error_fraction[1, ], simplify = F))
comp_ef = comp_error_fraction[rep(1, each = nrow(csn_daily_conc)), ]
setDT(comp_ef)

# assign data conditionally to concentration and uncertainty values for PMF
conc_pmf = csn_conc_mdl * csn_conc +
  (!csn_conc_mdl) * csn_mdl * 0.5
unc_pmf = csn_conc_mdl * (csn_mdl / 3 + comp_ef * csn_conc) +
  (!csn_conc_mdl) * 5/6 * csn_mdl
unc_pmf$PM25 = 3 * csn_daily_conc$PM25

###### EPA_MDL - signal-to-noise SNR S/N estimation ###### 
# according to EPA PMF 5.0 User Guide.pdf, function 5-3 & 5-4 from page 23
conc_unc_diff_pmf = (conc_pmf - unc_pmf)/unc_pmf * csn_conc_mdl
conc_unc_diff_pmf = cbind(csn_daily[, 1:4], conc_unc_diff_pmf)

# merge with cluster info
conc_unc_diff_pmf = merge(csn_cluster, conc_unc_diff_pmf)
conc_unc_diff_pmf$Date = conc_unc_diff_pmf$State = conc_unc_diff_pmf$Qualifier = NULL

# calculate SNR of each site 
site_snr = aggregate(. ~ as.factor(SiteCode), 
                     conc_unc_diff_pmf, 
                     sum) / 
  table(conc_unc_diff_pmf$SiteCode)
site_snr$`as.factor(SiteCode)` = NULL
site_snr$PM25 = -999

# calculate SNR of each cluster
cluster_snr = aggregate(. ~ as.factor(Final.Decision), 
                     conc_unc_diff_pmf, 
                     sum) / 
  table(conc_unc_diff_pmf$Final.Decision)
cluster_snr$`as.factor(Final.Decision)` = cluster_snr$SiteCode = NULL
cluster_snr$PM25 = -999

###### EPA_MDL - output files for all CSN conc, unc, snr etc. ###### 
# insert date into  concentration & uncertainty datasets
conc_pmf <- cbind(csn_daily[, 1:4], conc_pmf)
unc_pmf <- cbind(csn_daily[, 1:4], unc_pmf)

csn_conc_mdl_Site = cbind(csn_daily[, 1:4], csn_conc_mdl)

write.csv(conc_pmf, "CSN_concentration_for_PMF_EPAmdl.csv")
write.csv(unc_pmf, "CSN_uncertainty_for_PMF_EPAmdl.csv")
write.csv(csn_conc_mdl_Site, "CSN_concentration_vs_MDL_EPAmdl.csv")

write.csv(site_snr, "CSN_Site_SNR_for_PMF_EPAmdl.csv")
write.csv(cluster_snr, "CSN_Cluster_SNR_for_PMF_EPAmdl.csv")

###### EPA_MDL - daily PM of selected cluster - PMF priori try ###### 
# extract concentration & uncertainty of selected sites for PMF

# sites.top.PM.cluster
csn_select_conc = subset(conc_pmf, 
                         SiteCode %in% 
                           sites.top.PM.cluster)
csn_select_unc = subset(unc_pmf, 
                        SiteCode %in% 
                          sites.top.PM.cluster)

dim(csn_select_conc)
dim(csn_select_unc)

# sites.least.PM.cluster
csn_select_conc = subset(conc_pmf, 
                         SiteCode %in% 
                           sites.least.PM.cluster)
csn_select_unc = subset(unc_pmf, 
                        SiteCode %in% 
                          sites.least.PM.cluster)

csn_select_conc = csn_select_conc[!duplicated(csn_select_conc), ]
csn_select_unc = csn_select_unc[!duplicated(csn_select_unc), ]
dim(csn_select_conc)
dim(csn_select_unc)

# only use data before 2014 for priori try
csn_select_conc_use = subset(csn_select_conc, Date < as.Date("2014-01-01"))
csn_select_unc_use = subset(csn_select_unc, Date < as.Date("2014-01-01"))
csn_select_site_row = data.frame(table(csn_select_conc_use$SiteCode))
colnames(csn_select_site_row)[1] = c("SiteCode")

csn_select_conc_use = csn_select_conc_use[!duplicated(csn_select_conc_use), ]
csn_select_unc_use = csn_select_unc_use[!duplicated(csn_select_unc_use), ]
dim(csn_select_conc_use)
dim(csn_select_unc_use)

# extract conc & unc data for selected sites
#site 3 & 4 for cluster with top PM and 2&3 for cluster with the lowest PM
csn_select_conc_1site = subset(csn_select_conc_use, 
                               SiteCode == csn_select_site_row$SiteCode[3]) 
csn_select_unc_1site = subset(csn_select_unc_use, 
                              SiteCode == csn_select_site_row$SiteCode[3]) 
dim(csn_select_conc_1site)
dim(csn_select_unc_1site)

csn_select_conc_use$State = csn_select_unc_use$State = 
  csn_select_conc_1site$State = csn_select_unc_1site$State = 
  csn_select_conc_use$Qualifier = csn_select_unc_use$Qualifier = 
  csn_select_conc_1site$Qualifier = csn_select_unc_1site$Qualifier = NULL

csn_select_conc_1site.1 = csn_select_conc_1site[!duplicated(csn_select_conc_1site), ]
csn_select_unc_1site.1 = csn_select_unc_1site[!duplicated(csn_select_unc_1site), ]
dim(csn_select_conc_1site.1)
dim(csn_select_unc_1site.1)

write.csv(csn_select_conc_use, "CSN_Cluster6_2011-14_PMF_conc_EPAmdl.csv")
write.csv(csn_select_unc_use, "CSN_Cluster6_2011-14_PMF_unc_EPAmdl.csv")
write.csv(csn_select_site_row, "CSN_Cluster6_date_number_of_site_EPAmdl.csv")

write.csv(csn_select_conc_1site, "CSN_Cluster6_site3_2011-14_PMF_conc_EPAmdl.csv") #site 3
write.csv(csn_select_unc_1site, "CSN_Cluster6_site3_2011-14_PMF_unc_EPAmdl.csv") #site 3

write.csv(csn_select_conc_1site, "CSN_Cluster6_site4_2011-14_PMF_conc_EPAmdl.csv")
write.csv(csn_select_unc_1site, "CSN_Cluster6_site4_2011-14_PMF_unc_EPAmdl.csv")

write.csv(csn_select_conc_use, "CSN_Cluster24_2011-14_PMF_conc_EPAmdl.csv")
write.csv(csn_select_unc_use, "CSN_Cluster24_2011-14_PMF_unc_EPAmdl.csv")
write.csv(csn_select_site_row, "CSN_Cluster24_date_number_of_site_EPAmdl.csv")

write.csv(csn_select_conc_1site, "CSN_Cluster24_site3_2011-14_PMF_conc_EPAmdl.csv") #site 3
write.csv(csn_select_unc_1site, "CSN_Cluster24_site3_2011-14_PMF_unc_EPAmdl.csv") #site 3

write.csv(csn_select_conc_1site, "CSN_Cluster24_site2_2011-14_PMF_conc_EPAmdl.csv")
write.csv(csn_select_unc_1site, "CSN_Cluster24_site2_2011-14_PMF_unc_EPAmdl.csv")

#### convert file to fit for CMD running ####
# read conc & unc data
conc_site = read.csv("CSN_Cluster6_site3_2011-14_PMF_conc_EPAmdl.csv")
unc_site = read.csv("CSN_Cluster6_site3_2011-14_PMF_unc_EPAmdl.csv")
conc_site$X = unc_site$X = NULL

# read data about when the conc is above the unc
csn_conc_mdl_Site = read.csv("CSN_concentration_vs_MDL_EPAmdl.csv")
csn_conc_mdl_Site$X = NULL
csn_conc_mdl_Site$Date = as.Date(csn_conc_mdl_Site$Date)
study.site = csn_conc_mdl_Site$SiteCode[1]

# extract the conc_vs._mdl data of selected site(s)
conc_mdl_Site = subset(csn_conc_mdl_Site,
                       Date < as.Date("2014-01-01") &
                         SiteCode == study.site)
conc_mdl_Site$State = conc_mdl_Site$Qualifier = NULL

## checking the ratio of concentrations below MDL for PMF resetting for selected cluster
conc_belowMDL = data.frame(round(sapply(
  conc_mdl_Site[, 3:ncol(conc_mdl_Site)], sum)/
    nrow(conc_mdl_Site)*100, 0))
conc_belowMDL$CompName = rownames(conc_belowMDL)
colnames(conc_belowMDL)[1] = "Percent"
conc_belowMDL$Percent[conc_belowMDL$CompName == "S"] = 
  conc_belowMDL$Percent[conc_belowMDL$CompName == "SO4"] 

# plot the percent of below_MDL_concentrations for each species
ggplot(conc_belowMDL, aes(CompName, Percent)) +
  geom_point() +
  geom_hline(yintercept = 10, color = "red") +
  geom_hline(yintercept = 20, color = "orange", linetype='dotted') +
  annotate("text", x = "Feb", y = 20, label = "Weak  ↓", vjust = -0.5, col = "orange") +
  annotate("text", x = "Feb", y =10, label = "Bad  ↓", vjust = -0.5, col = "red") +
  ylab("Percent of below-EPA_MDL concentration") +
  theme_bw()

# generate the weak & bad species based on the conc_vs_mdl
species.weak.Pmdl = conc_belowMDL$CompName[
  conc_belowMDL$Percent <= 20 &
    conc_belowMDL$Percent > 10]
species.bad.Pmdl = conc_belowMDL$CompName[
  conc_belowMDL$Percent <= 10]
# add "S" into bad to exclude its co-linear effect with SO4
species.bad.Pmdl = append("S", species.bad) 

# get the SNR signal-to-noise ratio 
site_snr = read.csv("CSN_Site_SNR_for_PMF_EPAmdl.csv")
site_snr$X = site_snr$Final.Decision = NULL
snr_selected_site = subset(site_snr, 
                           SiteCode == study.site)

# generate the weak & bad species based on SNR
species.bad.snr = colnames(snr_selected_site)[which(
  colSums(snr_selected_site) < 0.1)]
species.weak.snr = colnames(snr_selected_site)[which(
  colSums(snr_selected_site) >= 0.1 &
    colSums(snr_selected_site) < 1)]

# combine the "weak" & "bad" list
species.bad = append(species.bad.snr, species.bad.Pmdl)
species.weak = append(species.weak.snr, species.weak.Pmdl)

# remove the duplicated strings from the character vector
species.bad = unique(unlist(strsplit(species.bad, " ")))
species.weak = unique(unlist(strsplit(species.weak, " ")))

# remove PM25, which potentially exists in the weak/bad list
species.bad = species.bad[! species.bad %in% "PM25"]
species.weak = species.weak[! species.weak %in% "PM25"]

# remove species marked as bad
conc_site_pmf = conc_site[ ,-which(
  names(conc_site) %in% species.bad)]
unc_site_pmf = unc_site[ ,-which(
  names(conc_site) %in% species.bad)]

# for those marked as weak, set the uncertainty 3 times as much
unc_site_pmf[names(unc_site_pmf) %in% species.weak] = 
  3 * unc_site_pmf[names(unc_site_pmf) %in% species.weak]

# add info into column names before combination
colnames(conc_site) = paste0("conc_", colnames(conc_site))
colnames(unc_site) = paste0("unc_", colnames(unc_site))

# combining conc & unc files
cmd_site_conc_unc = cbind(conc_site, unc_site)

# Interleave columns of concentration and uncertainty files
## generate a new vector to index the columns of cbind file
interleave.s <- rep(1:ncol(unc_site), each = 2) + (0:1) * ncol(unc_site)
## reorder the cbind file
cmd_site_interleave = cmd_site_conc_unc[interleave.s]

# only keep one Date column
cmd_site_interleave$conc_Date = NULL
colnames(cmd_site_interleave)[1] = "Date"
head(cmd_site_interleave)

# remove duplicated dates (for data till 2023.Feb, will update the data later)
cmd_site_final = cmd_site_interleave[!duplicated(cmd_site_interleave$Date), ]

write.csv(cmd_site_final, paste0("CSN_site_", study.site, "_conc_unc_PMF_CMD.csv"))

##### compare MDL - EPA vs. monthly #####
csn_mdl_epa$Source = "EPA"
csn_daily_fullMDL$Source = "Monthly_CSN"

csn_fullMDL_month = csn_daily_fullMDL
csn_fullMDL_month$Date = csn_fullMDL_month$SiteCode = NULL

csn_mdl_epa_plot = csn_mdl_epa
csn_fullMDL_month_plot = csn_fullMDL_month

csn_mdl_epa_plot = gather(csn_mdl_epa_plot, "Species", "MDL", -Source)
csn_fullMDL_month_plot = gather(csn_fullMDL_month_plot, "Species", "MDL", -Source)

csn_mdl_plot = rbind(csn_mdl_epa_plot, csn_fullMDL_month_plot)

ggplot(NULL, aes(Species, MDL)) +
  geom_boxplot(csn_fullMDL_month_plot) +
  geom_point(csn_mdl_epa_plot) +
  geom_line(csn_mdl_epa_plot) +
  ylab("Percent of above-EPA_MDL concentration") +
  theme_bw()

ggplot(subset(csn_mdl_plot, Species != "PM25"), 
       aes(Species, MDL, col = Source)) +
  geom_boxplot() +
  facet_grid(Source~., scales = "free") + 
  # scale_color_npg() + 
  theme_bw()

ggplot(subset(csn_mdl_plot, Species != "PM25"), 
       aes(Species, MDL, col = Source)) +
  geom_boxplot() +
  facet_grid(Source~., scales = "free") + 
  ylim(0, 0.1) +
  # scale_color_npg() + 
  theme_bw()
  
theme(plot.title = element_text(hjust = 0.05, vjust = 0, size = 16),
        strip.text.x = element_text(size = 20, colour = "Orange", angle = 0),
        legend.title = element_text(face="italic", size=16), # family="Times", colour="red", 
        legend.text = element_text(size=14), 
        axis.title.x = element_text(color="grey25", size = 20, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(color="grey25", size = 20, vjust=1, margin=margin(0,2,0,0)),
        axis.text.x = element_text(color="grey25", size = 14, angle = 0, hjust = 0.5, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.y = element_text(color="grey25", size = 14, angle = 0, hjust = 0.5))



