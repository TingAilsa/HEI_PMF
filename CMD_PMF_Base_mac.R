##clear environment
# rm(list=ls())

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/PMF_nonGUI_Cluster/base_DISPres1")
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

# library(gridExtra)

####### Read & process other files to use ####### 
# Directory containing the CSV files you want to read
# csv_folder <- "/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_txt_noCsub_noExtreme/other_files/"

cluster_info_all = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/PMF_NoGUI_NoCsub_NoExtreme_cluster/CSN_noCsub_noExtreme_PMF_CMD_StrongWeakBad_Cluster.csv")
species_class = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/CSN_Species_class_sub.csv")
cluster_info_all$X = species_class$X = NULL

cluster_info_all = plyr::rename(
  cluster_info_all, 
  c("K." = "KIon",
    "Na." = "NaIon", 
    "NH4." = "NH4Ion",
    "NO3" = "NO3Ion",
    "SO4" = "SO4Ion",
    "PM25" = "PM2.5"))

noCsub_noExtreme = "PMF_NoGUI_NoCsub_NoExtreme_cluster"
source_cluster = paste0("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/", 
                        noCsub_noExtreme)

####### START analyses ####### 

for (cluster.No in 25) { # 1:25
  for (factor.No in 10) { # 6:11
    
    ####### Read the results from bash script ####### 
    # Access the input file name passed as an argument
    folder_path <- paste0("Cluster_", cluster.No, "/Factor_", factor.No, "/")
    base_output = readLines(paste0(folder_path, 
                                   "CSN_noCsub_noExtreme_C_", 
                                   cluster.No, "_F_", factor.No,
                                   "_base.txt"))
    disp_output = readLines(paste0(folder_path, 
                                   "CSN_C_", 
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
    col_comp_all = col_comp(cluster_info, "Ag", "PM2.5")
    
    ## Select weak & strong variables by the value
    cluster.weak.strong = strong_weak(cluster_info, "Ag", "PM2.5")
    cluster.w.s.count = length(cluster.weak.strong)
    
    #cluster.strong = strong_species(cluster_info, "Ag", "PM2.5")
    #cluster.str.count = length(cluster.strong)
    
    #cluster.weak = weak_species(cluster_info, "Ag", "PM2.5")
    #cluster.weak.count = length(cluster.weak)
    
    
    ####### Extract info from PMF CMD outputs & match with date, PM #######
    
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
        paste0("CSN_noCsub_noExtreme_C_", cluster.No, "_SiteDate.csv")),
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
    
    
    ####### Source Assignment & Match #######
    #main3_species = Nmain_Species(base_percent, 3)
    main5_species = Nmain_Species(base_percent, 5)
    
    # Source detection according to info in recent publications
    main_source = source_ref(base_percent, 5)
    main_source$Source.No = sapply(strsplit(main_source$Source_reference, "-"), "[", 1)
    
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
      geom_errorbar(aes(ymin = Trans.Percent.down,
                        ymax = Trans.Percent.up), 
                    width = 0.4) +
      facet_grid(Source_reference ~ .) +
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
        plot.title = element_text(hjust = 0.05, vjust = -25, size = 16),
        # strip.background = element_blank(), strip.text = element_blank(),
        legend.position = "none"
      )
    
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
    
    name.prefix = paste0("CSN_noCsub_noExtreme_C_", cluster.No, "_F_", factor.No, "_")
    
    # ggsave(paste0(name.prefix, "source_profile.pdf"), plot = conc_percent_source, width = 6, height = 7.5)
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
    write.csv(lm_beta_plot_output, paste0(name.prefix, "overall.csv"))
  }
}


####### Merge files ####### 

library(pdftools)

for (cluster.No in 1:25) { # 1:25
  for (factor.No in 6:11) { # 6:11
    # Name pattern
    name.prefix = paste0("CSN_noCsub_noExtreme_C_", cluster.No, "_F_", factor.No, "_")
    
    # List all PDF files that match the pattern in the working directory
    pdf_files <- 
      list.files(
        pattern = paste0(name.prefix, 
                         "*.pdf"))
    
    # Combine them into a single PDF
    pdf_combine(pdf_files, 
                output = paste0(name.prefix, 
                                "combined.pdf"))
    
  }
}

# List all CSV files that match the pattern in the working directory
daily_csv_files <- list.files(pattern = "*daily.csv")

# Read and combine them into a single data frame
combined_daily <- do.call(rbind, 
                          lapply(daily_csv_files, 
                                 fread))

# combined_daily$X = NULL
combined_daily$Date = as.Date(combined_daily$Date)

# Write the combined CSV to a new file
write.csv(combined_daily, 
          file = "CSN_noCsub_noExtreme_combined_daily.csv", 
          row.names = FALSE)


# List all CSV files that match the pattern in the working directory
csv_files <- list.files(pattern = "*overall.csv")

# Read and combine them into a single data frame
combined_overall <- do.call(rbind, 
                            lapply(csv_files, 
                                   fread))

# Write the combined CSV to a new file
write.csv(combined_overall, 
          file = "CSN_noCsub_noExtreme_combined_overall.csv", 
          row.names = FALSE)

cty_rural_urban = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/CSN_IMPROVE_comp/IMPROVE_CSN_PopDensity_Urban_Rural_classify_331sites.csv")
cty_rural_urban$X = cty_rural_urban$X.1 = NULL
cty_rural_urban = cty_rural_urban[!duplicated(cty_rural_urban$SiteCode), ] 


combined_daily_use = subset(combined_daily, 
                            Source.No != "F")
combined_daily_use$SiteCode = as.character(combined_daily_use$SiteCode)

daily_contri_gps = merge(combined_daily_use, 
                         cty_rural_urban,
                         all.x = T)


site_contri_gps = ddply(daily_contri_gps, 
                        .(SiteCode, Source_reference),
                        summarise,
                        Longitude = mean(Longitude),
                        Latitude = mean(Latitude),
                        geoid = mean(geoid),
                        Contribution = mean(Contribution))

daily_contri_gps$geoid = ifelse(daily_contri_gps$geoid < 10000, 
                                paste0("0", daily_contri_gps$geoid), 
                                daily_contri_gps$geoid)

site_contri_gps$geoid = ifelse(site_contri_gps$geoid < 10000, 
                                paste0("0", site_contri_gps$geoid), 
                                site_contri_gps$geoid)


library(USAboundaries)
# generate the US county boundary data
us_cty_bdr = USAboundaries::us_counties()
us_cty_bdr[, 13] = NULL # there are two "state_name"
head(us_cty_bdr)
us_cty_bdr_geo = dplyr::select(us_cty_bdr, geoid, stusps, geometry)

daily_source_gps = merge(us_cty_bdr_geo, 
                         daily_contri_gps)
site_source_gps = merge(us_cty_bdr_geo, 
                        site_contri_gps)


site_Biomass = subset(site_source_gps, 
                      Source_reference == "F8-Biomass")
site_FreshSeaSalt = subset(site_source_gps, 
                           Source_reference == "F6-Fresh Sea Salt")
site_AgedSeaSalt = subset(site_source_gps, 
                          Source_reference == "F4-Aged Sea Salt")
site_SedNitrate = subset(site_source_gps, 
                         Source_reference == "F2-Secondary Nitrate")
site_SedSulfate = subset(site_source_gps, 
                         Source_reference == "F3-Secondary Sulfate")
site_SoilDust = subset(site_source_gps, 
                       Source_reference == "F9-Soil/Dust")


# MainStates <- map_data("state")
UScounty <- map_data("county")

site_Biomass_plot <-
  ggplot(site_Biomass, 
         aes(Longitude, Latitude, color= Contribution)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  ggtitle("Biomass") +
  # scale_color_manual(values = npg.color[1:20]) +
  theme_linedraw()

site_FreshSeaSalt_plot <-
  ggplot(site_FreshSeaSalt, 
         aes(Longitude, Latitude, color= Contribution)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  ggtitle("FreshSeaSalt") +
  # scale_color_manual(values = npg.color[1:20]) +
  theme_linedraw()

site_AgedSeaSalt_plot <-
  ggplot(site_AgedSeaSalt, 
         aes(Longitude, Latitude, color= Contribution)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  ggtitle("AgedSeaSalt") +
  # scale_color_manual(values = npg.color[1:20]) +
  theme_linedraw()

site_SedNitrate_plot <-
  ggplot(site_SedNitrate, 
         aes(Longitude, Latitude, color= Contribution)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  ggtitle("SedNitrate") +
  # scale_color_manual(values = npg.color[1:20]) +
  theme_linedraw()

site_SedSulfate_plot <-
  ggplot(site_SedSulfate, 
         aes(Longitude, Latitude, color= Contribution)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  ggtitle("SedSulfate") +
  # scale_color_manual(values = npg.color[1:20]) +
  theme_linedraw()

site_SoilDust_plot <-
  ggplot(site_SoilDust, 
         aes(Longitude, Latitude, color= Contribution)) + 
  geom_polygon(data=UScounty, aes(x=long, y=lat, group=group),
               color="lightgrey", fill="white", alpha = 0.4) +
  geom_point(size = 2, alpha = 0.6) +
  ggtitle("SoilDust") +
  # scale_color_manual(values = npg.color[1:20]) +
  theme_linedraw()


