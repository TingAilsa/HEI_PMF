####### Libraries to use ####### 
library(tidyr)
library(dplyr)
library(base)
library(ggplot2)
library(ggsci)
library(gridExtra)

####### Read & process other files to use ####### 
# Directory containing the CSV files you want to read
csv_folder <- "/projects/HAQ_LAB/tzhang/pmf_no_gui/CSN_CMD_txt_noCsub_noExtreme/other_files/"

cluster_site_date = read.csv("CSN_noCsub_noExtreme_cluster_site_date.csv")
cluster_info_all = read.csv("CSN_noCsub_noExtreme_PMF_CMD_StrongWeakBad_Cluster.csv")
species_class = read.csv("CSN_Species_class_sub.csv")
cluster_site_date$X = cluster_info_all$X = species_class$X = NULL

cluster_site_date$Date = as.Date(cluster_site_date$Date)

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

####### Setting for plotting ####### 

####### for bar start from a value less than 0 
# https://stackoverflow.com/questions/22295253/force-bars-to-start-from-a-lower-value-than-0-in-ggplot-geom-bar-in-r
require(scales)
mylog_trans <- function(base=exp(1), from=0) {
  trans <- function(x) log(x, base)-from
  inv <- function(x) base^(x+from)
  trans_new("mylog", trans, inv, log_breaks(base=base), 
            domain = c(base^from, Inf))
}

####### setting 30 colors for plotting clusters
npg.color = c("firebrick3", "indianred2", "lightskyblue", 
              "royalblue4", "lightseagreen", "skyblue3", 
              "goldenrod3", "burlywood4", 
              "burlywood3", "sandybrown", 
              "darkcyan")


#### Plot theme & function for font of ions

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

# theme for time series plots
theme.ts =   
  theme_bw() +
  theme(panel.grid = element_line(colour = "white"),
        plot.title = element_text(hjust = 0.05, vjust = -25, size = 16),
        # strip.background = element_blank(), strip.text = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(color="grey25", size = 12, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(color="grey25", size = 12, vjust=1, margin=margin(0,2,0,0)),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 11, angle = 0, hjust = 0.5, vjust = 0), 
        axis.text.y = element_text(color="grey25", size = 11, angle = 0, hjust = 0.5))


####### Functions for analyses #######

# Determine the line number in txt file including given string  
line_number <- function(lines, string) {
  # Loop through the lines to find the one containing the specific string
  for (i in seq_along(lines)) {
    if (grepl(string, lines[i])) {
      return(i) # Return the line number if found
    }
  }
  return(NULL) # Return NULL if not found
}

# Determine the task number of the lowest Qm from base PMF runs
# base_file = readLines("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_US_PMF/National_SA_PMF/CSN_PMF_noGUI_noCsub_AllData/CSN_C_6_F_9_2011-17_base_result.txt")

#### Transfer txt lines into dataframe
line_to_df <- function(lines_txt, nrow, ncol){
  # split the lines into individual elemenconc
  lines_txt = unlist(strsplit(lines_txt, "\\s+"))
  
  # create a matrix to store the individual elemenconc and convert to data.frame
  lines_df = data.frame(
    matrix(lines_txt, 
           nrow = nrow, 
           ncol = ncol, 
           byrow = T))
  
  return(lines_df)
}

#### Identify segments that are not spaces
non_space_segments <- function(line_to_replace) {
  # Split the line into segments (numbers and spaces)
  # (?<=...): a lookbehind assertion; (?=...): a lookahead assertion.
  # a space character (\\s), a non-space character (\\S)
  # together, splits the string exactly at the positions where a space transitions into a non-space or a non-space transitions into a space
  segments <- unlist(strsplit(line_to_replace, 
                              "(?<=\\s)(?=\\S)|(?<=\\S)(?=\\s)", 
                              perl = TRUE))
  
  # Identify segments that are not spaces
  non_space_segments <- sapply(segments, 
                               function(x) 
                                 !grepl("^\\s+$", x))
  
  return(non_space_segments)
}

lowest_Qm_task <- function(base_file) {
  
  ## Determine the number of lines to read
  # correlations was quoted with "correlations", use slash "\" to write a double quote character
  "factor \"correlations\" with Best-fit factors"
  line.start = line_number(base_file, 
                           "factor \"correlations\" with Best-fit factors")
  
  end.line = line.start + 20 
  start.line = line.start + 1 
  
  # Extract the lines including Q values and task numbers
  Q_lines <- base_file[start.line:end.line]
  
  # Convert the selected lines (with Q values & task number) into a data frame
  Q_task <- read.table(text = Q_lines, 
                       header = F, 
                       quote = "\"'")[, 1:4]
  colnames(Q_task) = c("TaskNum", "Qmain", "Qaux", "Q.XX")
  
  # Find the number of task when the value of Qm is the lowest
  lowest_Qm_task <- Q_task$TaskNum[which.min(Q_task$Qmain)]
  
  return(lowest_Qm_task)
}

#  function to replace a substring within a specific range in a string
substr_replace <- function(string, replacement, start, stop) {
  paste0(substr(string, 1, start - 1), 
         replacement, 
         substr(string, 
                stop + 1, 
                nchar(string)))
}

# function to replace the numoldsol
numoldsol_rp = 
  function(params_file, minQ_task_No) { # minQ_task_No is the task number with minimum Q value
    # Find the one containing "numoldsol"
    line.numoldsol = line_number(params_file, "numoldsol")
    
    # Get the next line (the one below "numoldsol")
    line_to_replace <- params_file[line.numoldsol + 1]
    
    # Find the position of the last space before the "0"
    position_to_replace <- regexpr("\\s0\\s*$", line_to_replace)
    
    # Replace the "0" with the replacement value if found
    if (position_to_replace > 0) {
      params_file[line.numoldsol + 1] <- substr_replace(
        line_to_replace, 
        minQ_task_No, 
        position_to_replace + 1, 
        position_to_replace + 1)
    }
    
    return(params_file)
  }

#### Identify columns with PM2.5 species and PM2.5 

col_comp = function(df, start.comp, end.comp) {
  
  start.col <- which(names(df) == start.comp)
  end.col <- which(names(df) == end.comp)
  col_comp = start.col:end.col
  
  return(col_comp)
}

#### Extract data in Base Model results for selected task number
# time series info from base output
base_results <- function(base_txt, task, cluster.row) {
  # Extract Factor matrix AA & BB from base result
  # the regular expression pattern
  pattern_start <- paste0("Results written by postprocessing for task #\\s+", task, "\\s+---------------------------")
  pattern_end <- paste0("Results written by postprocessing for task #\\s+", task+1, "\\s+---------------------------")
  
  # detect the line number where the pattern appears
  base.start.line = grep(pattern_start, base_output)
  base.end.line = grep(pattern_end, base_output)
  
  # data for the selected task number
  # the base run results for the selected task
  base_selectQtask = 
    base_output[
      (base.start.line+3):
        (base.end.line-4)]
  
  #### Extract fitted G vs. reference G Regression matrix
  # fitted G vs. reference G, regression
  G.correl.start = grep("Regression matrix T1 of fitted G vs. reference G:", 
                        base_selectQtask) + 1
  
  # Estimate the factor number
  # Split the line into segments (numbers and spaces)
  segments <- unlist(strsplit(base_selectQtask[G.correl.start], 
                              "(?<=\\s)(?=\\S)|(?<=\\S)(?=\\s)", 
                              perl = TRUE))
  
  # Identify segments that are not spaces
  non_space_segments <- sapply(segments, 
                               function(x) 
                                 !grepl("^\\s+$", x))
  
  # Factor number 
  factor.No <- sum(non_space_segments) - 1
  
  # fitted G vs. reference G regression matrix
  base_G_correl_txt = 
    base_selectQtask[
      G.correl.start : 
        (G.correl.start + factor.No - 1)]
  
  # split the lines into individual elements
  base_G_correl_txt = unlist(strsplit(base_G_correl_txt, "\\s+"))
  # create a matrix to store the individual elements and convert to data.frame
  base_G_cor = data.frame(
    matrix(base_G_correl_txt, 
           nrow = factor.No, 
           ncol = factor.No+2, 
           byrow = T))
  base_G_cor$X1 = NULL
  base_G_cor = mutate_all(base_G_cor, as.numeric)
  
  # rename the columns and replace the first column values
  colnames(base_G_cor)[1] = "Factors"
  colnames(base_G_cor)[2:(factor.No+1)] = Factor.serial
  base_G_cor$Factors = Factor.serial
  
  #### Extract time series data
  ts.start = grep("Factor matrix AA", 
                  base_selectQtask) + 1
  base_ts_txt = 
    base_selectQtask[
      ts.start : 
        (ts.start + cluster.row - 1)]
  
  # split the lines into individual elements
  base_ts_txt = unlist(strsplit(base_ts_txt, "\\s+"))
  
  # create a matrix to store the individual elements and convert to data.frame
  base_ts = data.frame(
    matrix(base_ts_txt, 
           nrow = cluster.row, 
           ncol = factor.No+2, 
           byrow = T))
  base_ts$X1 = NULL
  base_ts = mutate_all(base_ts, as.numeric)
  
  # rename the columns and replace the first column values
  colnames(base_ts)[1] = "Serial.No"
  colnames(base_ts)[2:(factor.No+1)] = Factor.serial
  
  #### Extract & estimate base concentration & percent 
  conc.start = grep("Factor matrix BB", 
                    base_selectQtask) + 1
  conc.end = grep("Factor matrix CC", 
                  base_selectQtask) - 4
  
  base_conc_txt = 
    base_selectQtask[
      conc.start:conc.end]
  
  # split the lines into individual elemenconc
  base_conc_txt = unlist(strsplit(base_conc_txt, "\\s+"))
  
  # create a matrix to store the individual elemenconc and convert to data.frame
  base_conc = data.frame(
    matrix(base_conc_txt, 
           nrow = conc.end - conc.start + 1, 
           ncol = factor.No+2, 
           byrow = T))
  sapply(base_conc, class)
  base_conc = mutate_all(base_conc, as.numeric)
  base_conc$X1 = NULL
  
  
  # rename the columns and replace the first column values
  colnames(base_conc)[1] = "Species"
  colnames(base_conc)[2:(factor.No+1)] = Factor.serial
  
  # Return a list containing both data frames
  return(list(base_G_cor = base_G_cor, 
              base_ts = base_ts, 
              base_conc = base_conc))
}

#### Match site & date, preparing for base model result plotting

time_series = function(base_ts, site_date){
  base_ts_all = base_ts_date = base_ts
  
  # match "Date", "SiteCode", "PM2.5", "State" info
  base_ts_all[c("Date", "SiteCode", "PM2.5", "State")] <- 
    site_date[c("Date", "SiteCode", "PM25", "State")]
  base_ts_all$species.sum = rowSums(base_ts_all[, 2:(factor.No+1)])
  sapply(base_ts_all, class)
  cor_PM_sumSpecies = cor(base_ts_all$PM2.5, 
                          base_ts_all$species.sum)
  
  base_ts_date[c("Date", "SiteCode")] <- 
    site_date[c("Date", "SiteCode")]
  base_ts_date$Serial.No = NULL
  
  #### 1. Gather the data for time series plotting
  base_ts_plot = gather(base_ts_date, 
                        "Factor", 
                        "Contribution", 
                        -Date, -SiteCode)
  
  #### 2. Linear regression resutls - contributions 
  ts_PM_lm = lm(PM2.5 ~ ., 
                data = base_ts_all[, 
                                   c("PM2.5", 
                                     paste0("Factor", 1:factor.No))])
  ts_PM_lm_beta = summary(ts_PM_lm)$coefficients[, 1]
  ts_PM_lm_beta = data.frame(ts_PM_lm_beta[2:length(ts_PM_lm_beta)])
  colnames(ts_PM_lm_beta) = "lm.beta.site"
  ts_PM_lm_beta$Factor = rownames(ts_PM_lm_beta)
  ts_PM_lm_beta$Factor.contribution = (ts_PM_lm_beta$lm.beta.site/
                                         sum(ts_PM_lm_beta$lm.beta.site))*100
  # keep three significant digits
  ts_PM_lm_beta$Factor.contr = paste0(
    signif(ts_PM_lm_beta$Factor.contribution, 
           3),
    "%")
  
  
  # Return a list containing both data frames
  return(list(base_ts_plot = base_ts_plot, 
              ts_PM_lm_beta = ts_PM_lm_beta))
}


#### concentration to & percent contribution

conc_percent_contri = function(conc_contribution_df){
  
  # get the percent contribution
  all_species = data.frame(Species = conc_contribution_df$Species)
  percent_value = 
    signif(
      base_conc[, -1] *100 / 
        rowSums(base_conc[, -1]), 
      2)
  percent_contribution = cbind(all_species, 
                               percent_value)
  
  return(percent_contribution)
}


#### results from DISP displacement analysis

disp_analysis = function(disp_output){
  # Extract lines with values
  disp_output_noNull = disp_output[! grepl('^\\s*$', disp_output)]
  
  # Get the second line in DISPres1.txt file to get the two numbers evaluating DISP performance
  DISPres1_sum = disp_output_noNull[1]
  
  # Split the string and remove empty elements
  disp.numbers <- strsplit(DISPres1_sum, "\\s+")
  disp.numbers <- disp.numbers[[1]][disp.numbers[[1]] != ""]
  
  # Convert to numeric
  disp.numbers <- as.numeric(disp.numbers)
  
  # Extract the two numbers
  disp.error.code <- disp.numbers[1]
  disp.qdrop <- disp.numbers[2]
  
  # Extract lines with values
  disp_output_matrix = disp_output_noNull[6:length(disp_output_noNull)]
  matrix_line = length(disp_output_matrix)/4
  
  # Extract up and down CI from DISP
  disp_down = disp_output_matrix[1:(matrix_line-1)]
  disp_up = disp_output_matrix[(matrix_line+1):(matrix_line*2-1)]
  
  # Identify segments that are not spaces
  info_segments = non_space_segments(disp_down[1])
  
  # Estimate the factor number
  factor.No <- sum(info_segments)
  
  Factor.serial = paste0("Factor", 1:factor.No)
  
  # lines to dataframe
  disp_down_df = line_to_df(disp_down, 
                            matrix_line - 1, 
                            factor.No + 1)
  disp_down_df = mutate_all(disp_down_df, as.numeric)
  colnames(disp_down_df)[1] = "Species"
  colnames(disp_down_df)[2:(factor.No+1)] = Factor.serial
  
  disp_up_df = line_to_df(disp_up, 
                          matrix_line - 1, 
                          factor.No + 1)
  disp_up_df = mutate_all(disp_up_df, as.numeric)
  colnames(disp_up_df)[1] = "Species"
  colnames(disp_up_df)[2:(factor.No+1)] = Factor.serial
  
  return(list(disp.error.code, disp.qdrop,
              disp_down = disp_down_df, 
              disp_up = disp_up_df))
}

#### N main species

Nmain_Species = function(percent_contribution, N){
  
  percent_contribution = subset(percent_contribution,
                                !(Species %in% c("PM25", "PM2.5")))
  
  rownames(percent_contribution) = percent_contribution[, 1]
  percent_contribution[, 1] = NULL
  
  # get the rownames of which the %ofSpecies ranks top N of the column
  N_main_Species = data.frame(
    apply(
      percent_contribution, 
      2, 
      function(x) 
        rownames(percent_contribution)
      [order(x, decreasing = T)[1:N]]
    ))
  
  # combine the selected rownames into one cell
  mainN_Species = data.frame(
    apply(N_main_Species, 2, 
          function(x) 
            paste(x, collapse = " ")
    ))
  
  colnames(mainN_Species)[1] = paste0("Main_Sepceis")
  
  # Making the row names as the first column by rearranging the columns
  mainN_Species$Factor = rownames(mainN_Species)
  rownames(mainN_Species) = NULL
  mainN_Species <- mainN_Species[, c(ncol(mainN_Species), 
                                     1:(ncol(mainN_Species) - 1))]
  
  return(mainN_Species)
}

source_ref = function(base_percent, N){
  main_species = Nmain_Species(base_percent, N)
  
  main_sources <- main_species %>%
    mutate(Source_reference = case_when(
      grepl("Al", Main_Species) & grepl("Si", Main_Species) & grepl("Ca", Main_Species) ~ "F9-Soil/Dust",
      grepl("NaIon", Main_Species) & grepl("Cl", Main_Species) ~ "F6-Fresh Sea Salt",
      grepl("Mg", Main_Species) & grepl("SO4Ion", Main_Species) ~ "F4-Aged Sea Salt",
      grepl("NH4Ion", Main_Species) & grepl("NO3Ion", Main_Species) ~ "F2-Secondary Nitrate",
      grepl("NH4Ion", Main_Species) & grepl("SO4Ion", Main_Species) ~ "F3-Secondary Sulfate",
      grepl("KIon", Main_Species) & grepl("OC", Main_Species) ~ "F8-Biomass",
      TRUE ~ "F-" # Default value if no condition is met
    ))
  
  return(main_sources)
}

####### Read the results from bash script ####### 
# Access the input file name passed as an argument
args <- commandArgs(trailingOnly = T)

base_file <- args[1]
cluster.No <- as.integer(args[2])
factor.No <- as.integer(args[3])
folder_path <- args[4]
disp_file <- args[5]

# Read the content of the input base file
base_output = readLines(base_file)
disp_output = readLines(disp_file)

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

cluster.strong = strong_species(cluster_info, "Ag", "PM2.5")
cluster.str.count = length(cluster.strong)

cluster.weak = weak_species(cluster_info, "Ag", "PM2.5")
cluster.weak.count = length(cluster.weak)


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
main3_species = Nmain_Species(base_percent, 3)
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
  geom_point(aes(y = exp(TransformedPercent)), color = "black", shape = 15) +
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
                        labels = c(0, 10, 20, 50, 99))
  ) +
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

data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results/results_figures/"

name.prefix = paste0("CSN_noCsub_noExtreme_C_", cluster.No, "_F_", factor.No, "_")

ggsave(paste(name.prefix, "source_profile.pdf"), plot = conc_percent_source, width = 6, height = 7.5)
ggsave(paste(name.prefix, "daily.pdf"), plot = daily_oneSite, width = 6, height = 7.5)
ggsave(paste(name.prefix, "month.pdf"), plot = month_cluster, width = 6, height = 7.5)
ggsave(paste(name.prefix, "annual.pdf"), plot = annual_cluster, width = 6, height = 7.5)
ggsave(paste(name.prefix, "overall.pdf"), plot = overall_contri, width = 12, height = 4.5)


####### Other tries #######

library(showtext)

# Use a font that supports the characters you're using (Arial Unicode should)
font_add("Arial Unicode MS", regular = "Arial Unicode MS.ttf")
showtext_auto()



# Combine the two plots
library(gridExtra) 
library(ggpubr)
ts_combine <- ggarrange(daily_oneSite, 
                        month_cluster, 
                        annual_cluster, 
                        ncol = 1, nrow = 3)

# Set xlab & ylab
ts_combine <- annotate_figure(ts_combine, 
                              left = textGrob("Contribution", rot = 90, vjust = 2, gp = gpar(cex = 1.3)))
print(ts_combine) 


grid.arrange(
  conc_percent_source, daily_oneSite, 
  ncol = 2,
  heights = unit(7.5, "in")
)

# Second Page
grid.newpage()
grid.arrange(
  annual_cluster, month_cluster, daily_oneSite,
  ncol = 3,
  heights = unit(7.5, "in")
)

# Third Page
grid.newpage()
grid.arrange(
  overall_contri,
  heights = unit(2, "in"),
  widths = unit(6, "in")
)

dev.off()


