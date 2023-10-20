##clear environment
# rm(list=ls())

##set working directory
# CSN
# setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/CSN_base_DISPres1")

# IMPROVE
# setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/IMPROVE_base_DISPres1")

getwd()
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/results_R_data"


####### Libraries to use ####### 
library(tidyr)
library(dplyr)
library(plyr)
library(base)
library(lubridate)
library(data.table)
library(ggplot2)
library(ggsci)
library(ggpattern)
library(ggthemes)
library(scales)

# library(gridExtra)

####### Read & process other files to use ####### 
# Directory containing the CSV files you want to read
# csv_folder <- "/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_txt_noCsub_noExtreme/other_files/"

# CSN
cluster_info_all = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/PMF_NoGUI_NoCsub_NoExtreme_cluster/CSN_noCsub_noExtreme_PMF_CMD_StrongWeakBad_Cluster.csv")
species_class = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/CSN_Species_class_sub.csv")
cluster_info_all$X = species_class$X = NULL

# CSN
noCsub_noExtreme = "CSN_NoGUI_NoCsub_NoExtreme_cluster"
data.prefix = "CSN_noCsub_noExtreme_C_"
disp.prefix = "CSN_C_"


# IMPROVE
cluster_info_all = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_data_prepare/IMPROVE_noCsub_noExtreme_PMF_CMD_StrongWeakBad_Cluster.csv")
species_class = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/CSN_Species_class_sub.csv")
cluster_info_all$X = species_class$X = NULL

# IMPROVE
noCsub_noExtreme = "IMPROVE_NoGUI_NoCsub_NoExtreme_cluster"
data.prefix = "IMPROVE_noCsub_noExtreme_C_"
disp.prefix = "IMPROVE_C_"


cluster_info_all = plyr::rename(
  cluster_info_all, 
  c("K." = "KIon",
    "Na." = "NaIon", 
    "NH4." = "NH4Ion",
    "NO3" = "NO3Ion",
    "SO4" = "SO4Ion",
    "PM25" = "PM2.5"))

source_cluster = paste0("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/", 
                        noCsub_noExtreme)

####### START analyses ####### 


for (cluster.No in 1:25) { # 1:25
  for (factor.No in 6:11) { # 6:11
    
    tryCatch({
      ####### Read the results from bash script ####### 
      # Access the input file name passed as an argument
      folder_path <- paste0("Cluster_", cluster.No, "/Factor_", factor.No, "/")
      base_output = readLines(paste0(folder_path, 
                                     data.prefix, 
                                     cluster.No, "_F_", factor.No,
                                     "_base.txt")) # "_base.txt"
      disp_output = readLines(paste0(folder_path, 
                                     disp.prefix, 
                                     cluster.No, "_F_", factor.No,
                                     "_DISPres1.txt"))
      
      # Find the number of task when the value of Qm is the lowest
      lowest_Qm_taskNo = lowest_Qm_task(base_output)
      
      Factor.serial = paste0("Factor", 1:factor.No)
      
      ####### Cluster-specific species strong, weak, bad #######
      cluster_info = subset(cluster_info_all, 
                            Finaly.Decision == cluster.No)
      cluster.data.row = cluster_info$cluster.row
      
      # detect the column range for PM species & PM2.5
      col_comp_all = col_comp(cluster_info, "Al", "PM2.5")
      
      ## Select weak & strong variables by the value
      cluster.weak.strong = strong_weak(cluster_info, "Al", "PM2.5")
      cluster.w.s.count = length(cluster.weak.strong)
      
      #cluster.strong = strong_species(cluster_info, "Al", "PM2.5")
      #cluster.str.count = length(cluster.strong)
      
      #cluster.weak = weak_species(cluster_info, "Al", "PM2.5")
      #cluster.weak.count = length(cluster.weak)
      
      
      ####### Extract base info from PMF CMD outputs & match with date, PM #######
      
      # Fitted G vs. reference G & Time series
      base_info = base_results(base_output, 
                               lowest_Qm_taskNo, 
                               cluster.data.row)
      base_G_cor = base_info$base_G_cor
      base_ts = base_info$base_ts
      base_conc = base_info$base_conc
      base_conc$Species = cluster.weak.strong
      base_percent = conc_percent_contri(base_conc)
      
      base_conc_plot = gather(base_conc,
                              "Factor", 
                              "Concentration", 
                              -Species)
      
      base_percent_plot = gather(base_percent,
                                 "Factor", 
                                 "Percent", 
                                 -Species)
      
      # Extract site & date info
      site_date_cluster = read.csv(
        file.path(
          source_cluster,
          paste0(data.prefix, cluster.No, "_SiteDate.csv")),
        header = T)
      
      base_ts_plot = time_series(base_ts, site_date_cluster)$base_ts_plot
      ts_PM_lm_beta = time_series(base_ts, site_date_cluster)$ts_PM_lm_beta
      
      # Get annual and seasonal contributions
      base_ts_plot <- base_ts_plot %>%
        mutate(Year = year(Date),
               Season = case_when(
                 month(Date) %in% c(12, 1, 2) ~ "Winter",
                 month(Date) %in% c(3, 4, 5) ~ "Spring",
                 month(Date) %in% c(6, 7, 8) ~ "Summer",
                 month(Date) %in% c(9, 10, 11) ~ "Fall"
               ),
               Month = month(Date))
      
      ts_annual_plot = ddply(base_ts_plot, 
                             .(Year, SiteCode, Factor),
                             summarise,
                             Contribution = mean(Contribution))
      
      ts_season_plot = ddply(base_ts_plot, 
                             .(Season, SiteCode, Factor),
                             summarise,
                             Contribution = mean(Contribution))
      
      ts_month_plot = ddply(base_ts_plot, 
                            .(Month, SiteCode, Factor),
                            summarise,
                            Contribution = mean(Contribution))
      
      ####### Extract DISP info from PMF CMD, and validation results #######
      
      # DISP results - Credible intervals
      disp_down_conc = disp_analysis(disp_output)$disp_down
      disp_up_conc = disp_analysis(disp_output)$disp_up
      disp_down_conc$Species = disp_up_conc$Species = cluster.weak.strong
      
      disp_down_percent = conc_percent_contri(disp_down_conc)
      disp_down_percent_plot = gather(disp_down_percent,
                                     "Factor", 
                                     "Percent.down", 
                                     -Species)
      
      disp_up_percent = conc_percent_contri(disp_up_conc)
      disp_up_percent_plot = gather(disp_up_percent,
                                   "Factor", 
                                   "Percent.up", 
                                   -Species)
      
      # DISP results - overall validation of results
      disp.error.code = disp_analysis(disp_output)[[1]]
      disp.qdrop = disp_analysis(disp_output)[[2]]
      
      
      ####### Source Assignment & Match #######
      #main3_species = Nmain_Species(base_percent, 3)
      main5_species = Nmain_Species(base_percent, 5)
      
      # Source detection according to info in recent publications
      main_source = source_ref(base_percent, 5)
      main_source$Source.No = sapply(strsplit(main_source$Source_reference, "-"), "[", 1)
      main_source$Factor_source = main_source$Factor
      main_source$Factor_source[main_source$Source_reference != "F-"] = 
        main_source$Source_reference[main_source$Source_reference != "F-"]
      
      # replace colnames in base_ts by the exact source
      colnames(base_ts)[2:(factor.No+1)] = main_source$Factor_source
      pairs(base_ts[, 2:(factor.No+1)], pch = 19)
      
      conc_percent_bsDisp = merge(base_conc_plot, base_percent_plot)
      conc_percent_bsDisp = merge(conc_percent_bsDisp, disp_down_percent_plot)
      conc_percent_bsDisp = merge(conc_percent_bsDisp, disp_up_percent_plot)
      conc_percent_bsDisp = merge(conc_percent_bsDisp, species_class)
      conc_percent_bsDisp = merge(conc_percent_bsDisp, main_source)
      
      ts_plot = merge(base_ts_plot, main_source)
      ts_annual_plot = merge(ts_annual_plot, main_source)
      ts_season_plot = merge(ts_season_plot, main_source)
      ts_month_plot = merge(ts_month_plot, main_source)
      lm_beta_plot = merge(ts_PM_lm_beta, main_source)
      
      ####### Plotting #######
      
      #### Source Profile - Concentration & percent contribution #### 
      conc_percent_bsDisp_use = subset(conc_percent_bsDisp, Source.No != "F")
      
      # Create a new variable for transformed Percent to match the concentration scale
      conc_percent_bsDisp_use$Trans.Percent <- log(conc_percent_bsDisp_use$Percent + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05)
      conc_percent_bsDisp_use$Trans.Percent.down <- log(conc_percent_bsDisp_use$Percent.down + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05)
      conc_percent_bsDisp_use$Trans.Percent.up <- log(conc_percent_bsDisp_use$Percent.up + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05)
      
      # Create the plot
      conc_percent_source <- 
        ggplot(conc_percent_bsDisp_use,
               aes(x = reorder(Species, sequence), 
                   group = Source_reference)) +
        # Bar plot for Concentration
        geom_bar(aes(y = Concentration, fill = Source_reference), 
                 stat = "identity", width = 0.6, alpha = 0.8) +
        # Point plot for transformed Percent
        geom_point(aes(y = exp(Trans.Percent)), color = "black", shape = 15) +
        geom_errorbar(aes(ymin = Trans.Percent.down, ymax = Trans.Percent.up), 
                     width = 0.4) +
        facet_grid(Source_reference ~ .) +
        ggtitle(paste0(name.prefix, 
                       ", Error.Code = ", disp.error.code, 
                       ", DISP.Qdrop = ", disp.qdrop)) + 
        scale_y_log10(
          name = "Concentration",
          limits = c(1e-05, 1e-01),
          breaks = c(1e-05, 1e-04, 1e-03, 1e-02, 1e-01),
          labels = c(1e-05, 1e-04, 1e-03, 1e-02, 1e-01),
          sec.axis = sec_axis(trans = ~log(. + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05),
                              name = "% of Species",
                              breaks = log(c(1, 10, 20, 50, 99) + 1) / log(100) * (log(1e-01) - log(1e-05)) + log(1e-05),
                              labels = c(0, 10, 20, 50, 99))) +
        scale_y_continuous(trans = mylog_trans(base=10, from=-5),
                           limits = c(1E-5, max(conc_percent_bsDisp$Concentration))) +
        scale_fill_npg() +
        theme_bw() +
        xlab(format_variable("PM25 Species")) +
        scale_x_discrete(labels = function(x) format_variable(x)) +
        them_text_speciesName +
        theme(
          panel.grid = element_line(colour = "white"),
          plot.title = element_text(hjust = 0.05, vjust = 0, size = 13.5),
          # strip.background = element_blank(), strip.text = element_blank(),
          legend.position = "none"
        )

      # conc_percent_source
      
      #### Time-series factor percent contribution #### 
      
      #### Daily
      daily_plot_use = subset(ts_plot, Source.No != "F")
      
      daily_oneSite <- 
        ggplot(subset(daily_plot_use,
                      SiteCode = unique(daily_plot_use$SiteCode)[2]), 
               # ggplot(daily_plot_use,
               aes(x = Date, y = Contribution, 
                   group = Source_reference, color = Source_reference)) +
        facet_grid(Source_reference ~., scales = "free_y") +
        geom_line(linewidth = 0.3, alpha = 0.8)+
        geom_point(size = 0.3, alpha = 0.4) +
        scale_color_npg() +
        theme.ts
      
      #### Annual
      annual_plot_use = subset(ts_annual_plot, Source.No != "F")
      
      annual_cluster <- 
        #  ggplot(subset(annual_plot_use,
        #                SiteCode = unique(annual_plot_use$SiteCode)[2]), 
        ggplot(annual_plot_use,
               aes(x = as.factor(Year), y = Contribution, 
                   color = Source_reference)) +
        facet_grid(Source_reference ~., scales = "free_y") +
        geom_boxplot(linewidth = 0.3, width = 0.5, alpha = 0.8) +
        scale_color_npg() +
        theme.ts
      
      #### Month
      month_plot_use = subset(ts_month_plot, Source.No != "F")
      
      month_cluster <- 
        #  ggplot(subset(month_plot_use,
        #                SiteCode = unique(month_plot_use$SiteCode)[2]), 
        ggplot(month_plot_use,
               aes(x = as.factor(Month), y = Contribution, 
                   color = Source_reference)) +
        facet_grid(Source_reference ~., scales = "free_y") +
        geom_boxplot(linewidth = 0.3, width = 0.5, alpha = 0.8) +
        scale_color_npg() +
        theme.ts
      
      #### Overall factor percent contribution #### 
      lm_beta_plot_use = subset(lm_beta_plot, 
                                Source.No != "F")
      
      overall_contri <-
        ggplot(lm_beta_plot_use, 
               aes(x = Source.No, y = Factor.contribution)) +
        geom_bar_pattern(aes(fill = Source.No),
                         stat = "identity",
                         pattern_color = "white",
                         pattern_fill = "white",
                         # alpha = 0.8,
                         width = 0.3)  +
        geom_text(aes(label = paste(Source_reference, Factor.contr)), 
                  size = 6, angle = 90, hjust = 0, vjust = -3,
                  position = position_stack(vjust = 0)) + # start from same bottom level
        scale_fill_npg() +
        scale_y_continuous(position = "right") +
        theme_bw() +
        theme(panel.grid.major.x = element_line(colour="grey60", linetype="dashed"),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              legend.position = "none",
              axis.text.x = element_text(color="grey25", size = 16, angle = 90, hjust = 0.5, vjust = 0.5), 
              axis.text.y = element_text(color="grey25", size = 16, angle = 90, hjust = 5, vjust = -0.5),
              axis.title.x = element_text(color="grey25", size = 22, angle = 180, hjust = 0.5),
              axis.title.y = element_text(color="grey25", size = 0, angle = -90, vjust = -1))
      
      ####### Output files #######
      
      name.prefix = paste0(data.prefix, cluster.No, "_F_", factor.No, "_")
      
      ggsave(paste0(name.prefix, "daily.pdf"), plot = daily_oneSite, width = 6, height = 7.5)
      ggsave(paste0(name.prefix, "month.pdf"), plot = month_cluster, width = 6, height = 7.5)
      ggsave(paste0(name.prefix, "annual.pdf"), plot = annual_cluster, width = 6, height = 7.5)
      ggsave(paste0(name.prefix, "overall.pdf"), plot = overall_contri, width = 12, height = 4.5)
      
      conc_percent_bsDisp_output = conc_percent_bsDisp
      ts_plot_output = ts_plot
      lm_beta_plot_output = lm_beta_plot
      
      conc_percent_bsDisp_output$Cluster.No = ts_plot_output$Cluster.No = lm_beta_plot_output$Cluster.No = cluster.No
      conc_percent_bsDisp_output$Factor.No = ts_plot_output$Factor.No = lm_beta_plot_output$Factor.No = factor.No
      
      write.csv(conc_percent_bsDisp_output, paste0(name.prefix, "source_profile.csv"))
      write.csv(ts_plot_output, paste0(name.prefix, "daily.csv"))
      write.csv(ts_annual_plot, paste0(name.prefix, "annual.csv"))
      write.csv(ts_month_plot, paste0(name.prefix, "month.csv"))
      write.csv(lm_beta_plot_output, paste0(name.prefix, "overall.csv"))
      
      # this one lead to unexpected error
      ggsave(paste0(name.prefix, "source_profile.pdf"), plot = conc_percent_source, width = 6, height = 7.5)
      
    }, error=function(e) {
      print(paste("Error at iteration", i, ":", e$message))
    })
  }
}


###########################################################################
####### Merge files and National #######
###########################################################################

library(sf)
library(dplyr)
library(readr)

# Define directory (you can change this to your directory path)
dir_path <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/annual_results"
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/annual_results")


##### Merge files #####

# List all CSV files in the directory that end with "annual.csv"
all_files <- list.files(dir_path, pattern = ".*annual\\.csv$", full.names = TRUE)

# Filter out files that don't match the specific naming patterns
filtered_files <- grep(".*_noCsub_noExtreme_C_\\d+_F_\\d+_annual\\.csv$", all_files, value = TRUE)

# Function to read and append Dataset, Cluster, and Factor columns
read_and_append_info <- function(file_name) {
  df <- read.csv(file_name)
  df$X1 = df$X = NULL
  
  df$SiteCode = as.character(df$SiteCode)
  df = subset(df, Source.No != "F")
  
  # Extract dataset, cluster and factor numbers from the filename using regex
  dataset_name <- gsub("([^_]*)_noCsub_noExtreme_.*", "\\1", basename(file_name))
  cluster_num <- as.numeric(gsub(".*_C_([0-9]+)_F_.*", "\\1", file_name))
  factor_num <- as.numeric(gsub(".*_F_([0-9]+)_.*", "\\1", file_name))
  
  # Add these extracted values as new columns to the dataframe
  df$Dataset <- dataset_name
  df$Cluster.No <- cluster_num
  df$Factor.No <- factor_num
  
  return(df)
}

# Apply function to each file and combine them
combined_annual <- bind_rows(lapply(filtered_files, 
                                    read_and_append_info))

# If you want to save the combined data
write_csv(combined_annual, "combined_annual_data.csv")


##### National level Plotting #####

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


library(USAboundaries)
# generate the US county boundary data
us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr[, 13] = NULL # there are two "state_name"
head(us_cty_bdr)
us_cty_bdr_geo = dplyr::select(us_cty_bdr, geoid, stusps, geometry)

# MainStates <- map_data("state")
UScounty <- map_data("county")

us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr <- us_cty_bdr[!(us_cty_bdr$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

annual_source_gps = merge(us_cty_bdr_geo, 
                          annual_contri_gps)

annual_Biomass = subset(annual_source_gps, 
                        Source_reference == "F8-Biomass")
annual_FreshSeaSalt = subset(annual_source_gps, 
                             Source_reference == "F6-Fresh Sea Salt")
annual_AgedSeaSalt = subset(annual_source_gps, 
                            Source_reference == "F4-Aged Sea Salt")
annual_SedNitrate = subset(annual_source_gps, 
                           Source_reference == "F2-Secondary Nitrate")
annual_SedSulfate = subset(annual_source_gps, 
                           Source_reference == "F3-Secondary Sulfate")
annual_SoilDust = subset(annual_source_gps, 
                         Source_reference == "F9-Soil/Dust")

###### 1. Annual changes in 6 sources ######
color_npg = pal_npg("nrc")(10)

ggplot(annual_source_gps,
       aes(as.factor(Year), Contribution),
       color = Source_reference) +
  geom_boxplot(aes(color = Source_reference), 
               outlier.shape = NA, 
               linewidth = 0.3, width = 0.5) +
  geom_jitter(aes(color = Source_reference), 
              width = 0.15, alpha = 0.15)+
  facet_grid(Source_reference ~., 
             scales = "free_y") +
  ylim(0,5) +
  scale_y_continuous(breaks = c(0, 2, 4)) +
  xlab("Year") +
  scale_color_manual(values = color_npg[-c(1, 5, 7)]) +
  theme_bw() +
  theme(panel.grid = element_line(colour = "white"),
        # plot.title = element_text(hjust = 0.05, vjust = -25, size = 0),
        strip.background = element_blank(), strip.text = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(color="grey25", size = 20, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(color="grey25", size = 20, vjust=1, margin=margin(0,2,0,0)),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 18, angle = 90, hjust = 0.5, vjust = 0), 
        axis.text.y = element_text(color="grey25", size = 18, angle = 0, hjust = 0.5))


###### 2. Spatial distribution of sources in 2011 & 2020 ######

show_col(color_npg)

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = subset(annual_SedNitrate, 
                           Year %in% c(2011, 2019)), 
             aes(x = Longitude, y = Latitude, color = Contribution), # alpha = Contribution, 
             size = 4, alpha = 0.8) +
  scale_alpha_continuous(range = c(0.1, 0.99), guide = "legend") +
  scale_color_gradient(low = "white", high = color_npg[2]) +
  coord_sf(datum = NA) +
  facet_wrap(~Year, ncol = 1) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "transparent"))

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = subset(annual_SedSulfate, 
                           Year %in% c(2011, 2019)), 
             aes(x = Longitude, y = Latitude, color = Contribution),
             size = 4, alpha = 0.8) +
  scale_alpha_continuous(range = c(0.1, 0.99), guide = "legend") +
  scale_color_gradient(low = "white", high = color_npg[3]) +
  coord_sf(datum = NA) +
  facet_wrap(~Year, ncol = 1) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "transparent"))

ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = subset(annual_SoilDust, 
                           Year %in% c(2011, 2019)), 
             aes(x = Longitude, y = Latitude, color = Contribution),
             size = 4, alpha = 0.8) +
  scale_alpha_continuous(range = c(0.1, 0.99), guide = "legend") +
  scale_color_gradient(low = "white", high = color_npg[9]) +
  coord_sf(datum = NA) +
  facet_wrap(~Year, ncol = 1) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "transparent"))


summary(subset(annual_SedNitrate, Year == 2011)$Contribution)
summary(subset(annual_SedNitrate, Year == 2019)$Contribution)
summary(subset(annual_SedNitrate, Year == 2020)$Contribution)
summary(subset(annual_SedSulfate, Year == 2011)$Contribution)
summary(subset(annual_SedSulfate, Year == 2019)$Contribution)
summary(subset(annual_SedSulfate, Year == 2020)$Contribution)
summary(subset(annual_Biomass, Year == 2011)$Contribution)
summary(subset(annual_Biomass, Year == 2019)$Contribution)
summary(subset(annual_Biomass, Year == 2020)$Contribution)
summary(subset(annual_FreshSeaSalt, Year == 2011)$Contribution)
summary(subset(annual_FreshSeaSalt, Year == 2019)$Contribution)
summary(subset(annual_FreshSeaSalt, Year == 2020)$Contribution)
summary(subset(annual_AgedSeaSalt, Year == 2011)$Contribution)
summary(subset(annual_AgedSeaSalt, Year == 2019)$Contribution)
summary(subset(annual_AgedSeaSalt, Year == 2020)$Contribution)
summary(subset(annual_SoilDust, Year == 2011)$Contribution)
summary(subset(annual_SoilDust, Year == 2019)$Contribution)
summary(subset(annual_SoilDust, Year == 2020)$Contribution)

###### 3. Spatial distribution of Changes between 2011 & 2020 ######

# Load the dplyr package
library(dplyr)

# Filter the dataset for the years 2011 and 2019
annual_source_selectd <- annual_source_gps %>% 
  filter(Year %in% c(2011, 2019))

map_info = select(annual_source_selectd,
                  SiteCode, Longitude, Latitude,
                  geoid, state_abbr, geometry)

# Calculate the differences in Contribution for each Source_reference and SiteCode
contribution_diff <- 
  annual_source_selectd %>%
  dplyr::group_by(Source_reference, SiteCode) %>%
  dplyr::summarise(
    diff_contribution = ifelse(n() > 1, diff(Contribution), NA),
    Longitude = last(Longitude),
    Latitude = last(Latitude),
    geoid = last(geoid),
    state_abbr = last(state_abbr),
    # geometry = last(geometry),
    .groups = 'drop'  # This will automatically ungroup the data
  )


contribution_diff$col = contribution_diff$row = 1

contribution_diff$row[
  contribution_diff$Source_reference %in%
    c("F6-Fresh Sea Salt", "F8-Biomass", "F9-Soil/Dust")] = 2

contribution_diff$col[
  contribution_diff$Source_reference %in%
    c("F3-Secondary Sulfate", "F8-Biomass")] = 2

contribution_diff$col[
  contribution_diff$Source_reference %in%
    c("F4-Aged Sea Salt", "F9-Soil/Dust")] = 3

# Create a new variable to uniquely identify each facet
contribution_diff <- contribution_diff %>%
  mutate(facet_id = interaction(col, row, lex.order = TRUE))

# Create the plot
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = contribution_diff, 
             aes(x = Longitude, y = Latitude, alpha = diff_contribution, color = as.factor(facet_id)),
             size = 4) +
  scale_alpha_continuous(range = c(0.1, 0.99)) +
  scale_color_manual(values = color_npg[-c(1, 5, 7)]) +
  coord_sf(datum = NA) +
  facet_wrap(facet_id ~ .) +
  theme_minimal() +
  theme(# strip.text = element_text(color = "transparent"),
        panel.background = element_blank())


ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = contribution_diff, 
             aes(x = Longitude, y = Latitude, alpha = diff_contribution, color = as.factor(facet_id)),
             size = 4) +
  scale_alpha_continuous(range = c(0.1, 0.99)) +
  scale_color_manual(values = color_npg[-c(1, 5, 7)]) +
  coord_sf(datum = NA) +
  facet_wrap(facet_id ~ .) +
  theme_minimal() +
  theme(# strip.text = element_text(color = "transparent"),
    panel.background = element_blank())


contribution_diff <- contribution_diff %>%
  mutate(facet_id = interaction(Source_reference, col, row, lex.order = TRUE))

# Create the plot
ggplot() +
  geom_sf(data = us_states, fill = "grey96", alpha = 0.8) +
  geom_point(data = subset(contribution_diff, 
                           !is.na(diff_contribution)), 
             aes(x = Longitude, y = Latitude, 
                 color = diff_contribution),
             size = 2.5, alpha = 0.8) +
  scale_color_gradient2(low = color_npg[2], 
                        high = color_npg[8], 
                        midpoint = 0) +
  coord_sf(datum = NA) +
  facet_wrap(~ facet_id, 
             labeller = labeller(facet_id = 
                                   as_labeller(as.character, 
                                               default = label_value))) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        strip.text = element_text(color = "black", size = 16))


###### 3. Temporal trends for differnt parts of the mainland US ######

# Define the grouping for the regions using state abbreviations
state_regions <- tibble(
  state_abbr = c(
    "CT", "ME", "MA", "NH", "RI", "VT", # New England
    "NJ", "NY", "PA", # Mid-Atlantic
    "DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV", # South Atlantic
    "AL", "KY", "MS", "TN", # East South Central
    "AR", "LA", "OK", "TX", # West South Central
    "IL", "IN", "MI", "OH", "WI", # East North Central
    "IA", "KS", "MN", "MO", "NE", "ND", "SD", # West North Central
    "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", # Mountain
    "CA", "OR", "WA" # Pacific
  ),
  region = rep(c("New England", "Mid-Atlantic", "South Atlantic", "East South Central", 
                 "West South Central", "East North Central", "West North Central",
                 "Mountain", "Pacific"), c(6, 3, 8, 4, 4, 5, 7, 8, 3))
)


state_regions <- tibble(
  state_abbr = c(
    "WA", "OR", "ID", # Northwest
    "MT", "WY", "ND", "SD", # Northern Rockies
    "MN", "WI", "MI", "IA", "IL", # Upper Midwest
    "VT", "NH", "ME", "MA", "CT", "RI", "NY", # Northeast
    "CA", "NV", "UT", "CO", # West
    "AZ", "NM", "TX", "OK", # Southwest
    "AR", "LA", "MS", "AL", # South
    "MO", "KS", "NE", "KY", "IN", "OH", "WV", # Ohio Valley
    "VA", "NC", "SC", "GA", "FL", "TN", "DE", "MD" # Southeast
  ),
  region = rep(c("Northwest", "Northern Rockies", "Upper Midwest", "Northeast", 
                 "West", "Southwest", "South", "Ohio Valley", "Southeast"),
               c(3, 4, 5, 7, 4, 4, 4, 7, 8))
)


us_states_region = merge(us_states, state_regions)
annual_Biomass_region = merge(annual_Biomass, state_regions)

# Map
biomass_point_region <-
  ggplot() +
  geom_sf(data = us_states_region, aes(fill = region), color = "white") +
  geom_point(data = subset(annual_Biomass_region, Year %in% c(2011)), 
             aes(x = Longitude, y = Latitude),
             color = "white", alpha = 0.5, size = 2) +
  theme_minimal() +
  scale_fill_npg() +
  theme(legend.position = "bottom") +
  facet_wrap(~ region, ncol = 3) +
  theme_map() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text( size = 0))

ggsave("map_points_biomass.pdf", plot = biomass_point_region, width = 16, height = 12)


# Calculate the centroid of each region
# Compute the centroid for each region
region_centroids <- 
  us_states_region %>%
  group_by(region) %>%
  summarize(geometry = 
              st_centroid(st_union(geometry)), 
            .groups = "keep")

ggplot() +
  geom_sf(data = us_states_region, aes(fill = region), 
          color = "white") +
  geom_point(data = subset(annual_Biomass_region, Year %in% c(2011)), 
             aes(x = Longitude, y = Latitude),
             color = "white", alpha = 0.5, size = 2) +
  geom_text(data = labels_df, aes(x = x, y = y, label = region), size = 4) +
  theme_map() +
  scale_fill_npg() +
  theme(legend.position = "bottom")

# Temporal trends for each region
annual_Biomass_region_plot = 
  ddply(annual_Biomass_region, 
        .(region, Year, Source_reference),
        summarise,
        med.contri = median(Contribution, na.rm = T),
        up.contri = quantile(Contribution, 0.975),
        down.contri = quantile(Contribution, 0.025),
        Longitude = last(Longitude),
        Latitude = last(Latitude),
        geoid = last(geoid),
        state_abbr = last(state_abbr)
  )

ggplot(annual_Biomass_region_plot, 
       aes(x = Year, y = med.contri)) +
  geom_line(color = "red") +
  geom_point(shape = 3) +
  geom_errorbar(aes(ymin = down.contri, ymax = up.contri), width = 0.2) +
  facet_wrap(~ region, ncol = 3, scales='free') +
  scale_x_continuous(breaks = c(2011, 2014, 2017, 2020)) +
  # scale_y_continuous(breaks = c(0, 1, 2, 3)) +
  labs(title = "Annual Change in Biomass Contributions for Each Region",
       x = "Year",
       y = "Median Contribution") +
  # theme_minimal() +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text( size = 16), # facet text, face = "bold",
        axis.text.x = element_text(size = 14, hjust = 0.5, vjust = 0), 
        axis.text.y = element_text(size = 14, hjust = 0.5),
        axis.title.y = element_text(size = 15, hjust = 0.5, angle = 90))
  
