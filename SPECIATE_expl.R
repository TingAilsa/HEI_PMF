# install.packages("RODBC", repos = "https://cloud.r-project.org/")
# install.packages("RODBC", dependencies = TRUE)
# install.packages("~/Downloads/RODBC_1.3-20.tar", type = "source")
# library(RODBC)
# library(RODBCext)

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/SPECIATE")
getwd()
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/SPECIATE"

#### RODBC ####

# https://github.com/microsoft/homebrew-mssql-preview/issues/2

#### odbc package, 3 tries, 3 times all failed, see notes ####
library(odbc)
library(DBI)

# path to speciate.accdb
speciate_acc = "/Users/TingZhang/Documents/HEI HAQ PMF/SPECIATE/SPECIATE_5.2_8-1-2022.accdb"

# Create the connection string
# try 1, below code is for WS system
con_str <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
                  "Dbq=", speciate_acc, ";",
                  "Uid=Admin;Pwd=;")
# try 2, below will use the MDBTools newly created with Homebrew
# however, not able to fine libmdbodbc.dylib under /opt/homebrew/lib even after re-installation
con_str <- paste0("Driver={MDBTools};",
                  "Dbq=", speciate_acc, ";",
                  "Uid=;Pwd=;")

# try 3, Use the execute_mdb_query function to run SQL queries against the Access database file:
library(RODBC)

execute_mdb_query <- function(db_path, query) {
  tmpfile <- tempfile(fileext = ".csv")
  system(paste("mdb-sql -HFp -d, -R\"", db_path, "\" -Q\"", query, "\" >", tmpfile))
  result <- read.csv(tmpfile, header = TRUE, stringsAsFactors = FALSE)
  file.remove(tmpfile)
  return(result)
}

# Get the table names
get_mdb_tables <- function(db_path) {
  tables_raw <- system(paste("mdb-tables", shQuote(db_path)), intern = TRUE)
  tables <- unlist(strsplit(tables_raw, " "))
  return(tables)
}

table_names <- get_mdb_tables(speciate_acc)

query <- "SELECT * FROM your_table_name"
# however, I don't know the table name
# Execute the query and store the result in a data frame
result <- execute_mdb_query(accdb_file, query)

## somehow, now I can sussessfully install and call the RODBC package

# Connect to the database
con <- dbConnect(odbc(), .connection_string = con_str)





