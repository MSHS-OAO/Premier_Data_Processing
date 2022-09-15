
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(xlsx)

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
# general improvement opportunity:
# can we update the paycode mapping file to indicate productive vs. non-prod?

  ## Premier Formatting ------------------------------------------------------
  char_len_dpt <- 15
  char_len_dpt_name <- 50
  char_len_employ <- 30
  char_len_jobcode <- 10
  char_len_paycode <- 15

# Functions --------------------------------------------------------------
  import_recent_file <- function(folder.path, place) {
  #Importing File information from Folder
  File.Name <- list.files(path = folder.path, full.names = F)
  File.Path <- list.files(path = folder.path, full.names = T)
  File.Date <- as.Date(sapply(File.Name, function(x) substr(x,nchar(x)-14, nchar(x)-5)),
                       format = '%m_%d_%Y')
  File.Table <<- data.table::data.table(File.Name, File.Date, File.Path) %>%
    arrange(desc(File.Date))
  #Importing Data 
  data_recent <- read.table(File.Table$File.Path[place],
                            header = T,
                            as.is = T,
                            sep = '~',
                            fill = T)
                            # colClasses = "character")
                            # Home dept came in as numeric and was displaying
                            # as scientific, so tried bringing in as text
                            # since all other columns were text.
                            # Also found that some values are not including
                            # location and dept code

  # Also seeing:  
  # Warning message:
  #   In scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :
  #             EOF within quoted string

  return(data_recent)
  }

# Import Data -------------------------------------------------------------
bislr_payroll <- import_recent_file(paste0(dir_BISLR, '/Source Data'), 1)
  #quality check if correct file selected

# Import References -------------------------------------------------------
#delete start.end in mapping file and create in script to identify which paycycles to filter on in payroll files
  pay_cycles_uploaded <- read.xlsx(paste0(dir_BISLR,
                                        "/Reference",
                                        "/Pay cycles uploaded_Tracker.xlsx"),
                                 detectDates = T,
                                 sheetIndex = 1)
msus_removal_list <- read_xlsx(paste0(dir_BISLR,
                                      "/Reference/MSUS_removal_list.xlsx"),
                               sheet = 1)
#TBD references continued

# Data Processing -----------------------------------------------------------


# Creating Outputs --------------------------------------------------------


# Quality Checks -------------------------------------------------------


# Visualizations ----------------------------------------------------------


# Exporting Data ----------------------------------------------------------



