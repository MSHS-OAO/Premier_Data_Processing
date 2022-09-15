
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
  #Quality Check - Confirming Upload File
  cat('File selected is ',
      File.Table$File.Name[place],
      ". Is this the correct file?")
  answer <- select.list(choices = c("Yes", "No"),
                        preselect = "Yes",
                        multiple = F,
                        title = "Correct File?",
                        graphics = T)
  if (answer == "No") {
    user_selected_file <- select.list(choices = File.Table$File.Name,
                                multiple = F,
                                title = "Select the correct file",
                                graphics = T)
    place <- grep(user_selected_file, File.Table$File.Name)
    cat('File selected is ', File.Table$File.Name[place])
  }
  #Importing Data 
  data_recent <- read.csv(File.Table$File.Path[place],
                            header = T,
                            sep = '~',
                            fill = T)
  return(data_recent)
  }

# Import Data -------------------------------------------------------------
bislr_payroll <- import_recent_file(paste0(dir_BISLR, '/Source Data'), 1)

# Import References -------------------------------------------------------
pay_cycles_uploaded <- read.xlsx(paste0(dir_BISLR,
                                        "/Reference",
                                        "/Pay cycles uploaded_Tracker.xlsx"),
                                 sheetIndex = 1)
msus_removal_list <- read_xlsx(paste0(dir_BISLR,
                                      "/Reference/MSUS_removal_list.xlsx"),
                               sheet = 1)
  ## Universal Reference Files -----------------------------------------------
  map_uni_paycodes <- read_xlsx(paste0(dir_universal,
                                       "/Mapping/MSHS_Paycode_Mapping.xlsx"),
                                sheet = 1)
  map_uni_jobcodes <- read_xlsx(paste0(dir_universal,
                                       "/Mapping/MSHS_Jobcode_Mapping.xlsx"),
                                sheet = 1)
  map_uni_reports <- read_xlsx(paste0(dir_universal,
                                      "/Mapping/MSHS_Reporting_Definition_Mapping.xlsx"),
                               sheet = 1)

  ## Premier Reference Files -------------------------------------------------
  dict_premier_dpt <- read.csv(paste0(dir_universal,
                                      "/Premier/Dictionary Exports",
                                      "/DepartmentDictionary.csv"),
                               col.names = c('Corporation.Code',
                                             'Site',
                                             'Cost.Center',
                                             'Cost.Center.Description'),
                               sep = ",")
  map_premier_dpt <- read.csv(paste0(dir_universal,
                                     "/Premier/Mapping Exports",
                                     "/DepartmentMapping.csv"),
                              col.names = c('Effective.Date',
                                            'Corporation.Code',
                                            'Site',
                                            'Cost.Center',
                                            'Cost.Center.Map'),
                              sep = ",")
  dict_premier_jobcode <- read.csv(paste0(dir_universal,
                                          "/Premier/Dictionary Exports",
                                          "/DepartmentDictionary.csv"),
                                   col.names = c('Corporation.Code',
                                                 'Site',
                                                 'Cost.Center',
                                                 'Cost.Center.Description'),
                                   sep = ",")
  dict_premier_report <- read.csv(paste0(dir_universal,
                                         "/Premier/Dictionary Exports",
                                         "/DepartmentDef.csv"),
                                  col.names = c('Corporation.Code',
                                                'Site',
                                                'Report.Name',
                                                'Report.ID',
                                                'Cost.Center',
                                                'Report.Type',
                                                'Threshold.Type',
                                                'Target.Type',
                                                'Exclude.Report.Rollup',
                                                'Effective.Date',
                                                'Paycycle.Type',
                                                'Exclude.Admin.Rollup',
                                                'Exclude.Action.Plan',
                                                'blank14',
                                                'blank15',
                                                'blank16',
                                                'blank17'),
                                  sep = ",",
                                  fill = T)

# Data Processing -----------------------------------------------------------

  ## References --------------------------------------------------------------
  pay_cycles_uploaded <- pay_cycles_uploaded %>%
    mutate(Start_End = paste0(Start.Date, '-', End.Date))


# Creating Outputs --------------------------------------------------------


# Quality Checks -------------------------------------------------------


# Visualizations ----------------------------------------------------------


# Exporting Data ----------------------------------------------------------



