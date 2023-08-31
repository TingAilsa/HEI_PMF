# Load required library
library(stringr)

# Set the source and destination directories
source_dir <- "/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/National_SA_PMF/PMF_NoGUI_NoCsub_NoExtreme_cluster"
destination_base_dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/CSN_IMPROVE_comp/CSN_CMD_txt_noCsub_noExtreme/Cluster_"

# Get a list of all CSV files in the source directory
csv_files <- list.files(source_dir, pattern = "*.csv", full.names = TRUE)[1:25]

# Copy each CSV file to the respective cluster directory
for (file in csv_files) {
  # Extract the cluster number from the filename
  cluster_num <- str_extract(basename(file), "(?<=_C_)[0-9]+(?=_PMF_CMD.csv)")
  
  # Construct the destination directory path
  destination_dir <- paste0(destination_base_dir, cluster_num)
  
  # Copy the file
  file.copy(file, destination_dir)
}
