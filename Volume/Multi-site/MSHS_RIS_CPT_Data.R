
# Libraries  --------------------------------------------------------------
library(tidyverse)
library(readxl)

# Directories -------------------------------------------------------------
dir_data <- paste0('J:/deans/Presidents/SixSigma/MSHS Productivity',
                   '/Productivity/Volume - Data/Multisite Volumes',
                   '/Radiology RIS CPT Data')
dir_cdm <- paste0('J:/deans/Presidents/SixSigma/MSHS Productivity',
                  '/Productivity/Volume - Data/CDMs')
dir_universal <- paste0('J:/deans/Presidents/SixSigma/MSHS Productivity',
                        '/Productivity/Universal Data')

# Constants ---------------------------------------------------------------
diagnostic_scaling <- 2.69

# Import Data -------------------------------------------------------------
list_filenames_rawdata <- list.files(
  path = paste0(dir_data, "/Source Data"),
  full.names = T)

#Quality Check on # of files
if (length(list_filenames_rawdata) %% 11 != 0) {
  stop("Unexpected number of files in selected folder")
}

mshs_rad_data <- lapply(list_filenames_rawdata, read_xls)

mshs_or_rad_data <- read.csv(paste0(RIS_dir,"Charge Detail/OR Diagnostic/",
                                    "MSH_RIS_OR Diagnostic_",
                                    month_year,
                                    ".csv"), 
                             colClasses = c(rep("character",9))) %>%
  select(MRN, Name, ACC, Date, Exam, Exam.Modifier, Org, Resource)

# Import References -------------------------------------------------------
#Importing and formatting CDM
import_recent_cdm <- function(dir, site_cdm, file_type) {
  #Compiling Data on Files
  name <- list.files(path = dir, full.names = F, pattern = paste0(file_type, "$"))
  path <- list.files(path = dir, full.names = T, pattern = paste0(file_type, "$"))
  site <- sapply(name, function(x) unlist(str_split(x, pattern = "_"))[1])
  date_created <- sapply(name, function(x)
    unlist(str_split(x, pattern = "_"))[4])
  type <- sapply(name, function(x) unlist(str_split(x, pattern = "_"))[4])
  #Formatting Data
  date_created <- sapply(date_created, function(x) substr(x, 1, 9))
  date_created <- as.Date(date_created, "%d%B%Y")
  type <- sapply(type, function(x) substr(x, 11, nchar(x)))
  #Creating Table of Data
  files <- data.table::data.table(name, path, site, date_created, type)
  files <- files %>% arrange(desc(date_created)) %>% filter(site == site_cdm)
  #Selecting Most Recent File
  cdm_file_import <<- files[1, ]
  #Importing Data
  data_import <- read.csv(cdm_file_import$path, sep = ",", header = T,
                          na.strings = c("", "Unavailable", "VOIDV"), fill = T)
  #Processing Data
  data_export <- data_import %>%
    select(CHARGE_CODE, OPTB_cpt4, CHARGE_DESC, CHARGE_CLASS) %>%
    rename(`Charge Code` = CHARGE_CODE,
           `CPT Code` = OPTB_cpt4,
           `Charge Class` = CHARGE_CLASS) %>%
    mutate(`Charge Class` = as.numeric(`Charge Class`))
  return(data_export)
}

cdm_msmsw <- import_recent_cdm(paste0(dir_cdm, "/MSMW"), "SLR", "csv")
cat("CDM File Used:", cdm_file_import$name)
cdm_msbib <- import_recent_cdm(paste0(dir_cdm, "/BIB 2022 CDM"), "BI", "xlsx")
