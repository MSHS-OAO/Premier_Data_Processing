
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)

# Directories -------------------------------------------------------------
dir <- 'J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity'
dir_BISLR <- paste0(dir, '/Labor - Data/Multi-site/BISLR')
dir_universal <- paste0(dir, '/Universal Data')
  
# Constants ---------------------------------------------------------------
new_dpt_map <- 10095
map_effective_date <- as.Date('2022-01-01') #is this date ok?
accural_legacy_cc <- c(1109008600, 1109028600, 4409008600, 6409008600) #add other 8600, make quality check for new 8600, id errors non accural oracle but backmapped accural
productive_paycodes <- c('REGULAR', 'OVERTIME', 'EDUCATION', 'ORIENTATION',
                        'OTHER_WORKED', 'AGENCY')

  ## Premier Formatting ------------------------------------------------------
  char_len_dpt <- 15
  char_len_dpt_name <- 50
  char_len_employ <- 30
  char_len_jobcode <- 10
  char_len_paycode <- 15

# Functions --------------------------------------------------------------
#update read to accept txt file
  import_recent_file <- function(folder.path, place) {
  #Importing File information from Folder
  File.Name <- list.files(path = folder.path, full.names = F)
  File.Path <- list.files(path = folder.path, full.names = T)
  File.Date <- as.Date(sapply(File.Name, function(x) substr(x,nchar(x)-19, nchar(x)-10)),format = '%m_%d_%Y')
  File.Table <<- data.table::data.table(File.Name, File.Date, File.Path) %>%
    arrange(desc(File.Date))
  #Importing Data 
  data_recent <- read.xlsx(File.Table$File.Path[place], detectDates = T)
  data_recent <- data_recent %>% mutate(Source = File.Table$File.Path[place]) #File Source Column for Reference
  return(data_recent)
}

# Import Data -------------------------------------------------------------
  bislr_payroll <- import_recent_file(dir_BISLR, 1)
  #quality check if correct file selected

# Import References -------------------------------------------------------


# Data Processing -----------------------------------------------------------


# Creating Outputs --------------------------------------------------------


# Quality Checks -------------------------------------------------------


# Visualizations ----------------------------------------------------------


# Exporting Data ----------------------------------------------------------



