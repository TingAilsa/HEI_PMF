# R code for date mapping and replacement
equates_path_nc <- "/projects/HAQ_LAB/tzhang/haq_work/sumaiya_cmaq/equates_path_nc.txt"  
source_dates="/projects/HAQ_LAB/tzhang/haq_work/sumaiya_cmaq/emission_source_dates.txt"

date_replaced_path <- "/projects/HAQ_LAB/tzhang/haq_work/sumaiya_cmaq/emission_source_dates_path.txt"  

# R code for date mapping and replacement
equates_path_nc <- "/Users/TingZhang/Documents/HAQ Lab/Sumaiya/emission_source/equates_path_nc.txt"  
source_dates="/Users/TingZhang/Documents/HAQ Lab/Sumaiya/emission_source/emission_source_dates.txt"

date_replaced_path <- "/Users/TingZhang/Documents/HAQ Lab/Sumaiya/emission_source/emission_source_dates_path.txt"  


# Read the content of emission_source_dates.txt
emission_source_dates <- readLines(source_dates)

# Read the content of equates_path_nc.txt
equates_path_nc <- readLines(equates_path_nc)

# Create a dictionary (list) to map dates to paths
date_to_path <- 
  setNames(equates_path_nc, 
           sub(".*_([0-9]{8})_12US1_(?:.*\\.ncf(?:\\.gz)?)", 
               "\\1", 
               equates_path_nc))
# .*, infor before yyyymmdd; ([0-9]{8}), yyyymmdd
# .ncf(?:\\.gz)?, *.ncf or *.ncf.gz

print(length(date_to_path))
print(head(date_to_path))

# Function to replace dates with paths in a string
replace_date_with_path <- function(line) {
  # Split the line by comma
  dates_in_line <- unlist(strsplit(line, split = ",\\s*"))
  
  # For each date starting from the second one, replace it with its path
  for (i in 2:length(dates_in_line)) {
    if (dates_in_line[i] %in% names(date_to_path)) {
      dates_in_line[i] <- date_to_path[dates_in_line[i]]
    }
  }
  
  # Combine them back together
  return(paste(dates_in_line, collapse = ", "))
}

# Apply the replacement function to each line in emission_source_dates
emission_source_dates_path <- lapply(emission_source_dates, replace_date_with_path)

# Write the modified content to emission_source_dates_path.txt
writeLines(as.character(emission_source_dates_path), con = date_replaced_path)
