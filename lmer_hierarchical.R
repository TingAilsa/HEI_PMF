library(USAboundaries)
library(lme4)
library(sf)
library(dplyr)
library(plyr)
library(readr)
library(data.table)

# Define directory (you can change this to your directory path)
dir_path <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/annual_results"
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/annual_results")

#### data preparation ####
combined_annual = read.csv("combined_annual_data.csv")
cty_rural_urban = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$X = cty_rural_urban$X.1 = NULL
cty_rural_urban = cty_rural_urban[!duplicated(cty_rural_urban$SiteCode), ] 


annual_contri_gps = merge(combined_annual, 
                          cty_rural_urban,
                          by = "SiteCode",
                          all.x = T)


annual_contri_gps$geoid = ifelse(annual_contri_gps$geoid < 10000, 
                                 paste0("0", annual_contri_gps$geoid), 
                                 annual_contri_gps$geoid)

# generate the US county boundary data
us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr[, 13] = NULL # there are two "state_name"
head(us_cty_bdr)
us_cty_bdr_geo = dplyr::select(us_cty_bdr, geoid, stusps, geometry)

us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr <- us_cty_bdr[!(us_cty_bdr$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

annual_source_gps = merge(us_cty_bdr_geo, 
                          annual_contri_gps)
sapply(annual_source_gps, class)

#### site-specific annual PM2.5 ####
imp_daily = fread("/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp/IMPROVE_interpulation_random-forest_afterLog.csv")
imp_daily$V = imp_daily$X.1 = NULL
imp_daily$Date = as.Date(imp_daily$Date)

csn_daily_before = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/CSN_Interpolation/CSN_interpulation_random-forest_afterLog_before_2015.csv")
csn_daily_after = fread("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/CSN_Interpolation/CSN_interpulation_random-forest_afterLog_after_2015.csv")
csn_daily_before$V = csn_daily_after$V = NULL
csn_daily_before$Date = as.Date(csn_daily_before$Date)
csn_daily_after$Date = as.Date(csn_daily_after$Date)

imp_daily = select(imp_daily,
                   SiteCode, Date, PM2.5)

csn_daily_before = select(csn_daily_before,
                          SiteCode, Date, Accept.PM2.5)

csn_daily_after = select(csn_daily_after,
                         SiteCode, Date, PM2.5RC)

imp_daily$Year = year(imp_daily$Date)
csn_daily_before$Year = year(csn_daily_before$Date)
csn_daily_after$Year = year(csn_daily_after$Date)

colnames(csn_daily_before)[3] = colnames(csn_daily_after)[3] = "PM2.5"
csn_daily = rbind (csn_daily_before, csn_daily_after)

csn_annual = ddply(csn_daily, 
                   .(SiteCode, Year), 
                   summarise, 
                   PM2.5 = mean(PM2.5))
imp_annual = ddply(imp_daily, 
                   .(SiteCode, Year), 
                   summarise, 
                   PM2.5 = mean(PM2.5))

csn_imp_annual = rbind(csn_annual, imp_annual)

annual_source_gps = merge(annual_source_gps, 
                          csn_imp_annual,
                          all.x = T)

nm_annual_source <- annual_source_gps %>%
  mutate_if(is.numeric, function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  })
nm_annual_source$Year = annual_source_gps$Year
nm_annual_source$Cluster.No  = annual_source_gps$Cluster.No 
nm_annual_source$Factor.No = annual_source_gps$Factor.No
nm_annual_source$countyns = annual_source_gps$countyns


#### hierarchical analysis ####

hier_PMsource <- lmer(PM2.5 ~ Contribution * Year + # Source_reference * 
                        Longitude * Latitude * RuralUrban + 
                    (1|SiteCode), 
                  data = nm_annual_source)

hier_PMsource <- lmer(Contribution ~ Source_reference * Year + # Source_reference * Year * Region
                        Longitude * Latitude * RuralUrban + 
                        (1|SiteCode), 
                      data = nm_annual_source)

# global slope
# remove global intercept
# Year * Region trend
# model for each source

summary(hier_PMsource)


################################################
#### Assumed Lucas's code ####

library(lme4)

set.seed(123)  # Setting a seed for reproducibility

# Draft US40aq
US40aq <- expand.grid(
  Monitor  =  1:30, 
  Year  =  1:10, 
  Group  =  c("Group1", "Group2", "Group3", "Group4")
)

US40aq$Rdns <- rnorm(nrow(US40aq), mean = 5, sd = 2)
US40aq$Mkt <- rnorm(nrow(US40aq), mean = 20, sd = 5)  # average temperature in Celsius
US40aq$CGkt <- rnorm(nrow(US40aq), mean = 100, sd = 15)  # some arbitrary concentration US40aq

# Extend our previous US40aqset with meteorological variables
US40aq$Precipitation <- rnorm(nrow(US40aq), mean = 50, sd = 10)   # in mm
US40aq$RelHumidity <- rnorm(nrow(US40aq), mean = 70, sd = 10)     # in percentage
US40aq$Temperature <- rnorm(nrow(US40aq), mean = 20, sd = 5)      # in Celsius
US40aq$WindSpeed <- rnorm(nrow(US40aq), mean = 10, sd = 2)        # in km/h

head(US40aq)  


# Fit the model with meteorological variables and interactions
hier_rdns <- lmer(CGkt ~ Rdns * Year * Group + 
                    Precipitation * RelHumidity * Temperature * WindSpeed + 
                    (1|Monitor), 
                  data = US40aq)
summary(hier_rdns)
