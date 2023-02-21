##set working directory
setwd("/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE/PMF_output")
getwd()
data.dir <- "/Users/ztttttt/Documents/HEI PMF/CSN_IMPROVE/PMF_Results"

#### CSN EPA-MDL, Cluster6 & 7 factors ####
csn_eap_C6f7 = read.csv("EPA_MDL_CSN_Cluster6_F7.csv")
colnames(csn_eap_C6f7)[1] = "Date"

# there is formatting wrong in date due to influence of excel, so use the date from concentration file
csn_epa_C6f7_conc = read.csv("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/CSN_PMF_Try_data/EPA MDL/CSN_Cluster6_2011-14_PMF_conc_EPAmdl.csv")
csn_eap_C6f7$Date = csn_epa_C6f7_conc$Date
csn_eap_C6f7$Date = as.Date(csn_eap_C6f7$Date, format = "%m/%d/%y")

# match the total PM2.5 mass concentration
csn_eap_C6f7$P25 = csn_epa_C6f7_conc$PM25

# match the sidecode
site_row = read.csv("/Users/ztttttt/Dropbox/HEI_PMF_files_Ting/CSN_PMF_Try_data/EPA MDL/CSN_Cluster6_date_number_of_site_EPAmdl.csv")
csn_eap_C6f7$SiteCode = rep(unique(site_row$SiteCode), site_row$Freq)

csn_eap_C6f7 = csn_eap_C6f7[!duplicated(csn_eap_C6f7), ]
csn_eap_C6f7 <- csn_eap_C6f7[-2, ]

###### CSN EPA-MDL C6F7, lm, all sites ######
# use linear regression to get the factor contribution
csn_eap_C6f7_lm = lm(P25 ~ Factor.1 + Factor.2 + Factor.3 + Factor.4 + 
                       Factor.5 + Factor.6 + Factor.7, data = csn_eap_C6f7)
csn_eap_C6f7_lm_beta = summary(csn_eap_C6f7_lm)$coefficients[, 1]
csn_eap_C6f7_lm_beta = data.frame(csn_eap_C6f7_lm_beta[2:length(csn_eap_C6f7_lm_beta)])
colnames(csn_eap_C6f7_lm_beta) = "lm.factor.beta.cluster"
csn_eap_C6f7_lm_beta$Factor = rownames(csn_eap_C6f7_lm_beta)
csn_eap_C6f7_lm_beta$Factor.contribution = (csn_eap_C6f7_lm_beta$lm.factor.beta.cluster/
                                              sum(csn_eap_C6f7_lm_beta$lm.factor.beta.cluster))*100

write.csv(csn_eap_C6f7_lm_beta, "CSN_Cluster6_2011-14_EPAmdl_PMF_F7_overall.csv")

###### CSN EPA-MDL C6F7, lm, each sites ######
csn_eap_C6f7_site3 = subset(csn_eap_C6f7,
                            SiteCode == site_row$SiteCode[3])
csn_eap_C6f7_site3_lm = lm(P25 ~ Factor.1 + Factor.2 + Factor.3 + Factor.4 + 
                             Factor.5 + Factor.6 + Factor.7, 
                           data = csn_eap_C6f7_site3)
csn_eap_C6f7_site3_lm_beta = summary(csn_eap_C6f7_site3_lm)$coefficients[, 1]
csn_eap_C6f7_site3_lm_beta = data.frame(csn_eap_C6f7_site3_lm_beta[2:length(csn_eap_C6f7_site3_lm_beta)])
colnames(csn_eap_C6f7_site3_lm_beta) = "lm.beta.site-ClusterBased"
csn_eap_C6f7_site3_lm_beta$Factor = rownames(csn_eap_C6f7_site3_lm_beta)
csn_eap_C6f7_site3_lm_beta$Factor.contribution = (csn_eap_C6f7_site3_lm_beta$lm.beta.site/
                                                    sum(csn_eap_C6f7_site3_lm_beta$lm.beta.site))*100

write.csv(csn_eap_C6f7_site3_lm_beta, "CSN_Cluster6_2011-14_EPAmdl_PMF_F7_site3.csv")




