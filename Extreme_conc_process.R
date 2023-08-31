##clear environment
# rm(list=ls())

##set working directory
setwd("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/PMF_GUI_Cluster")
getwd()
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/R - original IMPROVE"

library(tidyverse)
library(readxl)
library(data.table)
library(dplyr)
library(plyr)
library(ggplot2) 
library(base)
library(ggrepel)
# library(bspec) # signal-to-noise ratio, snr{}

#### Plot theme & function for font of ions ####
# function to format ion variables
# \u208+#, subscript#; \u00B+#, superscript#; \u207A, \u207B, superscript +/-

# Here name the ions with "Ion" instead of adding symbols inside cause this cause confusion for gsub
format_variable <- function(variable) {
  variable <- gsub("ClIon", "Cl\u207B", variable)
  variable <- gsub("NO3Ion", "NO\u2083\u207B", variable)
  variable <- gsub("SO4Ion", "SO\u2084\u00B2\u207B", variable)
  variable <- gsub("CO3Ion", "CO\u2083\u00B2\u207B", variable)
  variable <- gsub("NH4Ion", "NH\u2084\u207A", variable)
  variable <- gsub("NaIon", "Na\u207A", variable)
  variable <- gsub("KIon", "K\u207A", variable)
  variable <- gsub("PM25", "PM\u2082.\u2085", variable)
  variable <- gsub("m3", "µm\u00B3", variable)
  return(variable)
}

# test the function
variable = c("ClIon", "NO3Ion", "SO4Ion", "CO3Ion", "NH4Ion", 
             "NaIon", "KIon", "Cl", "Na", "K")
format_variable(variable)

scatter.theme = theme(plot.title = element_text(hjust = 0.05, vjust = 0, size = 16),
                      strip.text.x = element_text(size = 20, colour = "Orange", angle = 0),
                      legend.title = element_text(face="italic", size=16), # family="Times", colour="red", 
                      legend.text = element_text(size=14), 
                      axis.title.x = element_text(color="grey25", size = 16, 
                                                  vjust=0, 
                                                  family = "Arial Unicode MS"), 
                      axis.title.y = element_text(color="grey25", size = 16, 
                                                  vjust=1, 
                                                  family = "Arial Unicode MS"),
                      axis.text.x = element_text(color="grey25", size = 14, 
                                                 angle = 0, 
                                                 hjust = 0.5, vjust = 0.3,
                                                 family = "Arial Unicode MS"), 
                      plot.margin = unit(c(2,1,2, 2), "lines"),
                      axis.text.y = element_text(color="grey25", size = 14, 
                                                 angle = 0, 
                                                 hjust = 0.5,
                                                 family = "Arial Unicode MS"))

#### read files ####
cluster_file = list.files("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/PMF_GUI_Cluster",
                          pattern = "*.csv")

#### Extreme analyses ####

for( i in 1:25){
  # conc & unc for each cluster
  conc_cluster = read.csv(cluster_file[1 + 3*(i-1)])
  unc_cluster = read.csv(cluster_file[3 + 3*(i-1)])
  dim(unc_cluster)
  conc_cluster$X = unc_cluster$X = NULL
  
  # rename columns for ions
  conc_cluster <- dplyr::rename(conc_cluster, 
                                "NO3Ion" = "NO3", 
                                "SO4Ion" = "SO4", 
                                "NH4Ion" = "NH4.",
                                "NaIon" = "Na.",
                                "KIon" = "K.")
 
  unc_cluster <- dplyr::rename(unc_cluster, 
                               "NO3Ion" = "NO3", 
                               "SO4Ion" = "SO4", 
                               "NH4Ion" = "NH4.",
                               "NaIon" = "Na.",
                               "KIon" = "K.")
  
  
  ###### determine the extremes ######
  summary(conc_cluster)
  # med_conc_cluster = sapply(conc_cluster[5:(ncol(conc_cluster)-1)], median)
  mean_conc_cluster = sapply(conc_cluster[5:(ncol(conc_cluster)-1)], mean)
  max_conc_cluster = sapply(conc_cluster[5:(ncol(conc_cluster)-1)], max)
  # med_conc_cluster*25 < max_conc_cluster
  
  # mean value and 25 times were selected due to the data observation.
  # however, for some site, mean * 20 may alreay miss some very high values 
  # for earth elements (Si, Al), mean * 20 may filter too many values
  extreme_conc_cluster = mean_conc_cluster * 25
  
  # identify rows in conc_cluster with values higher than the extreme values
  rows_extreme <- apply(conc_cluster[, 5:(ncol(conc_cluster)-1)], 1, 
                        function(row) 
                          any(row > extreme_conc_cluster))
  extreme.row.count = sum(rows_extreme)
  
  # extract the date and site infor of the rows with and without extreme values
  extreme_cluster = conc_cluster[rows_extreme, ]
  normal_cluster = conc_cluster[!rows_extreme, ]
  dim(extreme_cluster)
  dim(normal_cluster)
  
  ###### scatter plot - extremes & normal - Wildfire  ######
  Wildfire_extreme_lm = lm(KIon ~ OC, data = extreme_cluster)
  r2.wilWildfire.extreme <- summary(Wildfire_extreme_lm)$r.squared

  Wildfire_normal_lm = lm(KIon ~ OC, data = normal_cluster)
  r2.wilWildfire.normal <- summary(Wildfire_normal_lm)$r.squared
  
  Wildfire_normal <- 
    ggplot(Wildfire_normal_lm, 
           aes(KIon, OC)) +
    geom_point(color = "black") +
    # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
    stat_smooth(method = "lm", se = T, level = 0.25, 
                linetype = "dashed", color = "blue", 
                fill = "blue", alpha = 0.2) + 
    stat_smooth(method = "lm", se = T, level = 0.75, 
                linetype = "dashed", color = "red", 
                fill = "red", alpha = 0.2) +
    stat_smooth(method = "lm", se = T, level = 0.5, 
                color = "black") +
    annotate("text", x = Inf, y = Inf, 
             label = paste("y =", 
                           round(coef(Wildfire_normal_lm)[2],2), 
                           "*x +", 
                           round(coef(Wildfire_normal_lm)[1],2), 
                           "\nR^2 =",
                           round(r2.wilWildfire.normal, 2)),
             hjust = 1, vjust = 1.5, size = 5, col = "black") +
    ggtitle("Wildfire") +
    xlab(format_variable("KIon")) + 
    scatter.theme

  Wildfire_withExtreme <- 
    ggplot(Wildfire_normal_lm, 
           aes(KIon, OC)) +
    geom_point(color = "black") +
    geom_point(data = extreme_cluster, 
               aes(KIon, OC),
               color = "red") +
    # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
    stat_smooth(method = "lm", se = T, level = 0.25, 
                linetype = "dashed", color = "blue", 
                fill = "blue", alpha = 0.2) + 
    stat_smooth(method = "lm", se = T, level = 0.75, 
                linetype = "dashed", color = "red", 
                fill = "red", alpha = 0.2) +
    stat_smooth(method = "lm", se = T, level = 0.5, 
                color = "black") +
    annotate("text", x = Inf, y = Inf, 
             label = paste("y =", 
                           round(coef(Wildfire_normal_lm)[2],2), 
                           "*x +", 
                           round(coef(Wildfire_normal_lm)[1],2), 
                           "\nR^2 =",
                           round(r2.wilWildfire.normal, 2)),
             hjust = 1, vjust = 1.5, size = 5, col = "black") +
    ggtitle("Wildfire") +
    xlab(format_variable("KIon")) + 
    scatter.theme
  
  ###### scatter plot - extremes & normal - SecondNitrate  ######
  SecondNitrate_extreme_lm = lm(NO3Ion ~ NH4Ion, data = extreme_cluster)
  r2.wilSecondNitrate.extreme <- summary(SecondNitrate_extreme_lm)$r.squared
  
  SecondNitrate_normal_lm = lm(NO3Ion ~ NH4Ion, data = normal_cluster)
  r2.wilSecondNitrate.normal <- summary(SecondNitrate_normal_lm)$r.squared
  
 SecondNitrate_normal <- 
    ggplot(SecondNitrate_normal_lm, 
           aes(NO3Ion, NH4Ion)) +
    geom_point(color = "black") +
    # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
    stat_smooth(method = "lm", se = T, level = 0.25, 
                linetype = "dashed", color = "blue", 
                fill = "blue", alpha = 0.2) + 
    stat_smooth(method = "lm", se = T, level = 0.75, 
                linetype = "dashed", color = "red", 
                fill = "red", alpha = 0.2) +
    stat_smooth(method = "lm", se = T, level = 0.5, 
                color = "black") +
    annotate("text", x = Inf, y = Inf, 
             label = paste("y =", 
                           round(coef(SecondNitrate_normal_lm)[2],2), 
                           "*x +", 
                           round(coef(SecondNitrate_normal_lm)[1],2), 
                           "\nR^2 =",
                           round(r2.wilSecondNitrate.normal, 2)),
             hjust = 1, vjust = 1.5, size = 5, col = "black") +
    ggtitle("SecondNitrate") +
    xlab(format_variable("NO3Ion")) + 
    ylab(format_variable("NH4Ion")) + 
    scatter.theme
  
 SecondNitrate_withExtreme <- 
    ggplot(SecondNitrate_normal_lm, 
           aes(NO3Ion, NH4Ion)) +
    geom_point(color = "black") +
    geom_point(data = extreme_cluster, 
               aes(NO3Ion, NH4Ion),
               color = "red") +
    # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
    stat_smooth(method = "lm", se = T, level = 0.25, 
                linetype = "dashed", color = "blue", 
                fill = "blue", alpha = 0.2) + 
    stat_smooth(method = "lm", se = T, level = 0.75, 
                linetype = "dashed", color = "red", 
                fill = "red", alpha = 0.2) +
    stat_smooth(method = "lm", se = T, level = 0.5, 
                color = "black") +
    annotate("text", x = Inf, y = Inf, 
             label = paste("y =", 
                           round(coef(SecondNitrate_normal_lm)[2],2), 
                           "*x +", 
                           round(coef(SecondNitrate_normal_lm)[1],2), 
                           "\nR^2 =",
                           round(r2.wilSecondNitrate.normal, 2)),
             hjust = 1, vjust = 1.5, size = 5, col = "black") +
    ggtitle("SecondNitrate") +
    xlab(format_variable("NO3Ion")) + 
    ylab(format_variable("NH4Ion")) + 
    scatter.theme
  
  
 ###### scatter plot - extremes & normal - SecondSulfate  ######
 SecondSulfate_extreme_lm = lm(SO4Ion ~ NH4Ion, data = extreme_cluster)
 r2.wilSecondSulfate.extreme <- summary(SecondSulfate_extreme_lm)$r.squared
 
 SecondSulfate_normal_lm = lm(SO4Ion ~ NH4Ion, data = normal_cluster)
 r2.wilSecondSulfate.normal <- summary(SecondSulfate_normal_lm)$r.squared
 
 SecondSulfate_normal <- 
   ggplot(SecondSulfate_normal_lm, 
          aes(SO4Ion, NH4Ion)) +
   geom_point(color = "black") +
   # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
   stat_smooth(method = "lm", se = T, level = 0.25, 
               linetype = "dashed", color = "blue", 
               fill = "blue", alpha = 0.2) + 
   stat_smooth(method = "lm", se = T, level = 0.75, 
               linetype = "dashed", color = "red", 
               fill = "red", alpha = 0.2) +
   stat_smooth(method = "lm", se = T, level = 0.5, 
               color = "black") +
   annotate("text", x = Inf, y = Inf, 
            label = paste("y =", 
                          round(coef(SecondSulfate_normal_lm)[2],2), 
                          "*x +", 
                          round(coef(SecondSulfate_normal_lm)[1],2), 
                          "\nR^2 =",
                          round(r2.wilSecondSulfate.normal, 2)),
            hjust = 1, vjust = 1.5, size = 5, col = "black") +
   ggtitle("SecondSulfate") +
   xlab(format_variable("SO4Ion")) + 
   ylab(format_variable("NH4Ion")) + 
   scatter.theme
 
 SecondSulfate_withExtreme <- 
   ggplot(SecondSulfate_normal_lm, 
          aes(SO4Ion, NH4Ion)) +
   geom_point(color = "black") +
   geom_point(data = extreme_cluster, 
              aes(SO4Ion, NH4Ion),
              color = "red") +
   # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
   stat_smooth(method = "lm", se = T, level = 0.25, 
               linetype = "dashed", color = "blue", 
               fill = "blue", alpha = 0.2) + 
   stat_smooth(method = "lm", se = T, level = 0.75, 
               linetype = "dashed", color = "red", 
               fill = "red", alpha = 0.2) +
   stat_smooth(method = "lm", se = T, level = 0.5, 
               color = "black") +
   annotate("text", x = Inf, y = Inf, 
            label = paste("y =", 
                          round(coef(SecondSulfate_normal_lm)[2],2), 
                          "*x +", 
                          round(coef(SecondSulfate_normal_lm)[1],2), 
                          "\nR^2 =",
                          round(r2.wilSecondSulfate.normal, 2)),
            hjust = 1, vjust = 1.5, size = 5, col = "black") +
   ggtitle("SecondSulfate") +
   xlab(format_variable("SO4Ion")) + 
   ylab(format_variable("NH4Ion")) + 
   scatter.theme  
 
 ###### scatter plot - extremes & normal - Vehicle  ######
  Vehicle_extreme_lm = lm(OC ~ EC, data = extreme_cluster)
  r2.wilVehicle.extreme <- summary(Vehicle_extreme_lm)$r.squared
  
  Vehicle_normal_lm = lm(OC ~ EC, data = normal_cluster)
  r2.wilVehicle.normal <- summary(Vehicle_normal_lm)$r.squared
  
 Vehicle_normal <- 
    ggplot(Vehicle_normal_lm, 
           aes(OC, EC)) +
    geom_point(color = "black") +
    # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
    stat_smooth(method = "lm", se = T, level = 0.25, 
                linetype = "dashed", color = "blue", 
                fill = "blue", alpha = 0.2) + 
    stat_smooth(method = "lm", se = T, level = 0.75, 
                linetype = "dashed", color = "red", 
                fill = "red", alpha = 0.2) +
    stat_smooth(method = "lm", se = T, level = 0.5, 
                color = "black") +
    annotate("text", x = Inf, y = Inf, 
             label = paste("y =", 
                           round(coef(Vehicle_normal_lm)[2],2), 
                           "*x +", 
                           round(coef(Vehicle_normal_lm)[1],2), 
                           "\nR^2 =",
                           round(r2.wilVehicle.normal, 2)),
             hjust = 1, vjust = 1.5, size = 5, col = "black") +
    ggtitle("Vehicle") +
    xlab(format_variable("OC")) + 
    ylab(format_variable("EC")) + 
    scatter.theme
  
 Vehicle_withExtreme <- 
    ggplot(Vehicle_normal_lm, 
           aes(OC, EC)) +
    geom_point(color = "black") +
    geom_point(data = extreme_cluster, 
               aes(OC, EC),
               color = "red") +
    # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
    stat_smooth(method = "lm", se = T, level = 0.25, 
                linetype = "dashed", color = "blue", 
                fill = "blue", alpha = 0.2) + 
    stat_smooth(method = "lm", se = T, level = 0.75, 
                linetype = "dashed", color = "red", 
                fill = "red", alpha = 0.2) +
    stat_smooth(method = "lm", se = T, level = 0.5, 
                color = "black") +
    annotate("text", x = Inf, y = Inf, 
             label = paste("y =", 
                           round(coef(Vehicle_normal_lm)[2],2), 
                           "*x +", 
                           round(coef(Vehicle_normal_lm)[1],2), 
                           "\nR^2 =",
                           round(r2.wilVehicle.normal, 2)),
             hjust = 1, vjust = 1.5, size = 5, col = "black") +
    ggtitle("Vehicle") +
    xlab(format_variable("OC")) + 
    ylab(format_variable("EC")) + 
    scatter.theme  
  
  
 
 ###### scatter plot - extremes & normal - SoilDust  ######
 SoilDust_extreme_lm = lm(Si ~ Al, data = extreme_cluster)
 r2.wilSoilDust.extreme <- summary(SoilDust_extreme_lm)$r.squared
 
 SoilDust_normal_lm = lm(Si ~ Al, data = normal_cluster)
 r2.wilSoilDust.normal <- summary(SoilDust_normal_lm)$r.squared
 
 SoilDust_normal <- 
   ggplot(SoilDust_normal_lm, 
          aes(Si, Al)) +
   geom_point(color = "black") +
   # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
   stat_smooth(method = "lm", se = T, level = 0.25, 
               linetype = "dashed", color = "blue", 
               fill = "blue", alpha = 0.2) + 
   stat_smooth(method = "lm", se = T, level = 0.75, 
               linetype = "dashed", color = "red", 
               fill = "red", alpha = 0.2) +
   stat_smooth(method = "lm", se = T, level = 0.5, 
               color = "black") +
   annotate("text", x = Inf, y = Inf, 
            label = paste("y =", 
                          round(coef(SoilDust_normal_lm)[2],2), 
                          "*x +", 
                          round(coef(SoilDust_normal_lm)[1],2), 
                          "\nR^2 =",
                          round(r2.wilSoilDust.normal, 2)),
            hjust = 1, vjust = 1.5, size = 5, col = "black") +
   ggtitle("SoilDust") +
   xlab(format_variable("Si")) + 
   ylab(format_variable("Al")) + 
   scatter.theme
 
 SoilDust_withExtreme <- 
   ggplot(SoilDust_normal_lm, 
          aes(Si, Al)) +
   geom_point(color = "black") +
   geom_point(data = extreme_cluster, 
              aes(Si, Al),
              color = "red") +
   # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
   stat_smooth(method = "lm", se = T, level = 0.25, 
               linetype = "dashed", color = "blue", 
               fill = "blue", alpha = 0.2) + 
   stat_smooth(method = "lm", se = T, level = 0.75, 
               linetype = "dashed", color = "red", 
               fill = "red", alpha = 0.2) +
   stat_smooth(method = "lm", se = T, level = 0.5, 
               color = "black") +
   annotate("text", x = Inf, y = Inf, 
            label = paste("y =", 
                          round(coef(SoilDust_normal_lm)[2],2), 
                          "*x +", 
                          round(coef(SoilDust_normal_lm)[1],2), 
                          "\nR^2 =",
                          round(r2.wilSoilDust.normal, 2)),
            hjust = 1, vjust = 1.5, size = 5, col = "black") +
   ggtitle("SoilDust") +
   xlab(format_variable("Si")) + 
   ylab(format_variable("Al")) + 
   scatter.theme
 
 
 ###### scatter plot - extremes & normal - Industry, Mn~Cu  ######
 Industry_extreme_lm = lm(Mn ~ Cu, data = extreme_cluster)
 r2.wilIndustry.extreme <- summary(Industry_extreme_lm)$r.squared
 
 Industry_normal_lm = lm(Mn ~ Cu, data = normal_cluster)
 r2.wilIndustry.normal <- summary(Industry_normal_lm)$r.squared
 
 Industry_normal <- 
   ggplot(Industry_normal_lm, 
          aes(Mn, Cu)) +
   geom_point(color = "black") +
   # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
   stat_smooth(method = "lm", se = T, level = 0.25, 
               linetype = "dashed", color = "blue", 
               fill = "blue", alpha = 0.2) + 
   stat_smooth(method = "lm", se = T, level = 0.75, 
               linetype = "dashed", color = "red", 
               fill = "red", alpha = 0.2) +
   stat_smooth(method = "lm", se = T, level = 0.5, 
               color = "black") +
   annotate("text", x = Inf, y = Inf, 
            label = paste("y =", 
                          round(coef(Industry_normal_lm)[2],2), 
                          "*x +", 
                          round(coef(Industry_normal_lm)[1],2), 
                          "\nR^2 =",
                          round(r2.wilIndustry.normal, 2)),
            hjust = 1, vjust = 1.5, size = 5, col = "black") +
   ggtitle("Industry") +
   xlab(format_variable("Mn")) + 
   ylab(format_variable("Cu")) + 
   scatter.theme
 
 Industry_withExtreme <- 
   ggplot(Industry_normal_lm, 
          aes(Mn, Cu)) +
   geom_point(color = "black") +
   geom_point(data = extreme_cluster, 
              aes(Mn, Cu),
              color = "red") +
   # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
   stat_smooth(method = "lm", se = T, level = 0.25, 
               linetype = "dashed", color = "blue", 
               fill = "blue", alpha = 0.2) + 
   stat_smooth(method = "lm", se = T, level = 0.75, 
               linetype = "dashed", color = "red", 
               fill = "red", alpha = 0.2) +
   stat_smooth(method = "lm", se = T, level = 0.5, 
               color = "black") +
   annotate("text", x = Inf, y = Inf, 
            label = paste("y =", 
                          round(coef(Industry_normal_lm)[2],2), 
                          "*x +", 
                          round(coef(Industry_normal_lm)[1],2), 
                          "\nR^2 =",
                          round(r2.wilIndustry.normal, 2)),
            hjust = 1, vjust = 1.5, size = 5, col = "black") +
   ggtitle("Industry") +
   xlab(format_variable("Mn")) + 
   ylab(format_variable("Cu")) + 
   scatter.theme
 
 
 ###### scatter plot - extremes & normal - Industry, OC~Cu  ######
 Industry_normal_2_lm = lm(EC ~ Cu, data = normal_cluster)
 r2.wilIndustry.normal <- summary(Industry_normal_2_lm)$r.squared
 
 Industry_normal_2 <- 
   ggplot(Industry_normal_2_lm, 
          aes(EC, Cu)) +
   geom_point(color = "black") +
   # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
   stat_smooth(method = "lm", se = T, level = 0.25, 
               linetype = "dashed", color = "blue", 
               fill = "blue", alpha = 0.2) + 
   stat_smooth(method = "lm", se = T, level = 0.75, 
               linetype = "dashed", color = "red", 
               fill = "red", alpha = 0.2) +
   stat_smooth(method = "lm", se = T, level = 0.5, 
               color = "black") +
   annotate("text", x = Inf, y = Inf, 
            label = paste("y =", 
                          round(coef(Industry_normal_2_lm)[2],2), 
                          "*x +", 
                          round(coef(Industry_normal_2_lm)[1],2), 
                          "\nR^2 =",
                          round(r2.wilIndustry.normal, 2)),
            hjust = 1, vjust = 1.5, size = 5, col = "black") +
   ggtitle("Industry") +
   xlab(format_variable("EC")) + 
   ylab(format_variable("Cu")) + 
   scatter.theme
 
 Industry_withExtreme_2 <- 
   ggplot(Industry_normal_2_lm, 
          aes(EC, Cu)) +
   geom_point(color = "black") +
   geom_point(data = extreme_cluster, 
              aes(EC, Cu),
              color = "red") +
   # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
   stat_smooth(method = "lm", se = T, level = 0.25, 
               linetype = "dashed", color = "blue", 
               fill = "blue", alpha = 0.2) + 
   stat_smooth(method = "lm", se = T, level = 0.75, 
               linetype = "dashed", color = "red", 
               fill = "red", alpha = 0.2) +
   stat_smooth(method = "lm", se = T, level = 0.5, 
               color = "black") +
   annotate("text", x = Inf, y = Inf, 
            label = paste("y =", 
                          round(coef(Industry_normal_2_lm)[2],2), 
                          "*x +", 
                          round(coef(Industry_normal_2_lm)[1],2), 
                          "\nR^2 =",
                          round(r2.wilIndustry.normal, 2)),
            hjust = 1, vjust = 1.5, size = 5, col = "black") +
   ggtitle("Industry") +
   xlab(format_variable("EC")) + 
   ylab(format_variable("Cu")) + 
   scatter.theme
 

 ###### scatter plot - extremes & normal - Industry, Fe~Ba  ######
 Industry_normal_3_lm = lm(Fe ~ Ba, data = normal_cluster)
 r2.wilIndustry.normal <- summary(Industry_normal_3_lm)$r.squared
 
 Industry_normal_3 <- 
   ggplot(Industry_normal_3_lm, 
          aes(Fe, Ba)) +
   geom_point(color = "black") +
   # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
   stat_smooth(method = "lm", se = T, level = 0.25, 
               linetype = "dashed", color = "blue", 
               fill = "blue", alpha = 0.2) + 
   stat_smooth(method = "lm", se = T, level = 0.75, 
               linetype = "dashed", color = "red", 
               fill = "red", alpha = 0.2) +
   stat_smooth(method = "lm", se = T, level = 0.5, 
               color = "black") +
   annotate("text", x = Inf, y = Inf, 
            label = paste("y =", 
                          round(coef(Industry_normal_3_lm)[2],2), 
                          "*x +", 
                          round(coef(Industry_normal_3_lm)[1],2), 
                          "\nR^2 =",
                          round(r2.wilIndustry.normal, 2)),
            hjust = 1, vjust = 1.5, size = 5, col = "black") +
   ggtitle("Industry") +
   xlab(format_variable("Fe")) + 
   ylab(format_variable("Ba")) + 
   scatter.theme
 
 Industry_withExtreme_3 <- 
   ggplot(Industry_normal_3_lm, 
          aes(Fe, Ba)) +
   geom_point(color = "black") +
   geom_point(data = extreme_cluster, 
              aes(Fe, Ba),
              color = "red") +
   # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
   stat_smooth(method = "lm", se = T, level = 0.25, 
               linetype = "dashed", color = "blue", 
               fill = "blue", alpha = 0.2) + 
   stat_smooth(method = "lm", se = T, level = 0.75, 
               linetype = "dashed", color = "red", 
               fill = "red", alpha = 0.2) +
   stat_smooth(method = "lm", se = T, level = 0.5, 
               color = "black") +
   annotate("text", x = Inf, y = Inf, 
            label = paste("y =", 
                          round(coef(Industry_normal_3_lm)[2],2), 
                          "*x +", 
                          round(coef(Industry_normal_3_lm)[1],2), 
                          "\nR^2 =",
                          round(r2.wilIndustry.normal, 2)),
            hjust = 1, vjust = 1.5, size = 5, col = "black") +
   ggtitle("Industry") +
   xlab(format_variable("Fe")) + 
   ylab(format_variable("Ba")) + 
   scatter.theme
 
 
 ###### scatter plot - extremes & normal - FreshSeaSalte  ######
 FreshSeaSalte_extreme_lm = lm(Cl ~ Na, data = extreme_cluster)
 r2.wilFreshSeaSalte.extreme <- summary(FreshSeaSalte_extreme_lm)$r.squared
 
 FreshSeaSalte_normal_lm = lm(Cl ~ Na, data = normal_cluster)
 r2.wilFreshSeaSalte.normal <- summary(FreshSeaSalte_normal_lm)$r.squared
 
 FreshSeaSalte_normal <- 
   ggplot(FreshSeaSalte_normal_lm, 
          aes(Cl, Na)) +
   geom_point(color = "black") +
   # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
   stat_smooth(method = "lm", se = T, level = 0.25, 
               linetype = "dashed", color = "blue", 
               fill = "blue", alpha = 0.2) + 
   stat_smooth(method = "lm", se = T, level = 0.75, 
               linetype = "dashed", color = "red", 
               fill = "red", alpha = 0.2) +
   stat_smooth(method = "lm", se = T, level = 0.5, 
               color = "black") +
   annotate("text", x = Inf, y = Inf, 
            label = paste("y =", 
                          round(coef(FreshSeaSalte_normal_lm)[2],2), 
                          "*x +", 
                          round(coef(FreshSeaSalte_normal_lm)[1],2), 
                          "\nR^2 =",
                          round(r2.wilFreshSeaSalte.normal, 2)),
            hjust = 1, vjust = 1.5, size = 5, col = "black") +
   ggtitle("FreshSeaSalte") +
   xlab(format_variable("Cl")) + 
   ylab(format_variable("Na")) + 
   scatter.theme
 
 FreshSeaSalte_withExtreme <- 
   ggplot(FreshSeaSalte_normal_lm, 
          aes(Cl, Na)) +
   geom_point(color = "black") +
   geom_point(data = extreme_cluster, 
              aes(Cl, Na),
              color = "red") +
   # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
   stat_smooth(method = "lm", se = T, level = 0.25, 
               linetype = "dashed", color = "blue", 
               fill = "blue", alpha = 0.2) + 
   stat_smooth(method = "lm", se = T, level = 0.75, 
               linetype = "dashed", color = "red", 
               fill = "red", alpha = 0.2) +
   stat_smooth(method = "lm", se = T, level = 0.5, 
               color = "black") +
   annotate("text", x = Inf, y = Inf, 
            label = paste("y =", 
                          round(coef(FreshSeaSalte_normal_lm)[2],2), 
                          "*x +", 
                          round(coef(FreshSeaSalte_normal_lm)[1],2), 
                          "\nR^2 =",
                          round(r2.wilFreshSeaSalte.normal, 2)),
            hjust = 1, vjust = 1.5, size = 5, col = "black") +
   ggtitle("FreshSeaSalte") +
   xlab(format_variable("Cl")) + 
   ylab(format_variable("Na")) + 
   scatter.theme
 
 
 
 ###### scatter plot - extremes & normal - AgedSeaSalte  ######
 AgedSeaSalte_extreme_lm = lm(Mg ~ Na, data = extreme_cluster)
 r2.wilAgedSeaSalte.extreme <- summary(AgedSeaSalte_extreme_lm)$r.squared
 
 AgedSeaSalte_normal_lm = lm(Mg ~ Na, data = normal_cluster)
 r2.wilAgedSeaSalte.normal <- summary(AgedSeaSalte_normal_lm)$r.squared
 
 AgedSeaSalte_normal <- 
   ggplot(AgedSeaSalte_normal_lm, 
          aes(Mg, Na)) +
   geom_point(color = "black") +
   # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
   stat_smooth(method = "lm", se = T, level = 0.25, 
               linetype = "dashed", color = "blue", 
               fill = "blue", alpha = 0.2) + 
   stat_smooth(method = "lm", se = T, level = 0.75, 
               linetype = "dashed", color = "red", 
               fill = "red", alpha = 0.2) +
   stat_smooth(method = "lm", se = T, level = 0.5, 
               color = "black") +
   annotate("text", x = Inf, y = Inf, 
            label = paste("y =", 
                          round(coef(AgedSeaSalte_normal_lm)[2],2), 
                          "*x +", 
                          round(coef(AgedSeaSalte_normal_lm)[1],2), 
                          "\nR^2 =",
                          round(r2.wilAgedSeaSalte.normal, 2)),
            hjust = 1, vjust = 1.5, size = 5, col = "black") +
   ggtitle("AgedSeaSalte") +
   xlab(format_variable("Mg")) + 
   ylab(format_variable("Na")) + 
   scatter.theme
 
 AgedSeaSalte_withExtreme <- 
   ggplot(AgedSeaSalte_normal_lm, 
          aes(Mg, Na)) +
   geom_point(color = "black") +
   geom_point(data = extreme_cluster, 
              aes(Mg, Na),
              color = "red") +
   # geom_smooth(method = "lm", se = F, color = "red", formula = y ~ x) +
   stat_smooth(method = "lm", se = T, level = 0.25, 
               linetype = "dashed", color = "blue", 
               fill = "blue", alpha = 0.2) + 
   stat_smooth(method = "lm", se = T, level = 0.75, 
               linetype = "dashed", color = "red", 
               fill = "red", alpha = 0.2) +
   stat_smooth(method = "lm", se = T, level = 0.5, 
               color = "black") +
   annotate("text", x = Inf, y = Inf, 
            label = paste("y =", 
                          round(coef(AgedSeaSalte_normal_lm)[2],2), 
                          "*x +", 
                          round(coef(AgedSeaSalte_normal_lm)[1],2), 
                          "\nR^2 =",
                          round(r2.wilAgedSeaSalte.normal, 2)),
            hjust = 1, vjust = 1.5, size = 5, col = "black") +
   ggtitle("AgedSeaSalte") +
   xlab(format_variable("Mg")) + 
   ylab(format_variable("Na")) + 
   scatter.theme
  
  # site included in the cluster
  sites.in.cluster = unique(conc_cluster$SiteCode)
  
  # extract the conc_vs._mdl data of selected cluster(s)
  conc_mdl_cluster = subset(csn_conc_mdl_cluster,
                            SiteCode %in% sites.in.cluster)
  

  
  
  
}