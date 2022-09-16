
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
  ## OR Data -------------------------------------------------------------
  # mshs_or_rad_data <- read.csv(paste0(RIS_dir,"Charge Detail/OR Diagnostic/",
  #                                     "MSH_RIS_OR Diagnostic_",
  #                                     month_year,
  #                                     ".csv"),
  #                              colClasses = c(rep("character",9))) %>%
  #   select(MRN, Name, ACC, Date, Exam, Exam.Modifier, Org, Resource)
  import_recent_OR_file <- function(folder.path, place) {
    #Importing File information from Folder
    File.Name <- list.files(path = folder.path,pattern = 'csv$', full.names = F)
    File.Path <- list.files(path = folder.path,pattern = 'csv$', full.names = T)
    File.Date <- as.Date(sapply(File.Name, function(x) paste0('01', substr(x,nchar(x)-10, nchar(x)-4))),format = '%d%b%Y')
    File.Table <- data.table::data.table(File.Name, File.Date, File.Path) %>%
      arrange(desc(File.Date))
    all_dates <- File.Table %>% select(File.Date) %>% unique()
    File.Table <- File.Table %>% filter(File.Date %in% all_dates[place])
    cat("OR file selected for ",
        format(unique(File.Table$File.Date), format = '%B %Y'))
    #Importing Data 
    data_recent <- read.csv(File.Table$File.Path,
      colClasses = c(rep("character",9))) %>%
      select(MRN, Name, ACC, Date, Exam, Exam.Modifier, Org, Resource)
    return(data_recent)
  }
  mshs_or_rad_data <- import_recent_OR_file(paste0(dir_data, '/OR Source Data'), 1)
  
  ## RIS Data ----------------------------------------------------------------
  import_recent_RIS_files <- function(folder.path, place) {
    #Importing File information from Folder
    File.Name <- list.files(path = folder.path,pattern = 'xls$', full.names = F)
    File.Path <- list.files(path = folder.path,pattern = 'xls$', full.names = T)
    File.Date <- as.Date(sapply(File.Name, function(x) paste0('01', substr(x,nchar(x)-9, nchar(x)-4))),format = '%d%m%Y')
    File.Table <- data.table::data.table(File.Name, File.Date, File.Path) %>%
      arrange(desc(File.Date))
    all_dates <- File.Table %>% select(File.Date) %>% unique()
    File.Table <- File.Table %>% filter(File.Date %in% all_dates[place])
    #Quality Check on # of files
    if (nrow(File.Table) %% 11 != 0) {
      stop("Unexpected number of files in selected folder. There are ",
           nrow(File.Table),
           " files for ",
           format(unique(File.Table$File.Date), format = '%B %Y') ,
           " and there should be a total of 11 files for each month.")
    }
    File.Table <<- File.Table
    #Importing Data 
    data_recent <- lapply(File.Table$File.Path,read_xls)
  return(data_recent)
  }
  
  mshs_rad_data <- import_recent_RIS_files(paste0(dir_data, '/Source Data'), 1)
  cat('Data files selected for the month',
      format(unique(File.Table$File.Date), format = '%B %Y'))

# list_filenames_rawdata <- list.files(
#   path = paste0(dir_data, "/Source Data"),
#   pattern = 
#   full.names = T)
#Quality Check on # of files
# if (length(list_filenames_rawdata) %% 11 != 0) {
#   stop("Unexpected number of files in selected folder")
# }
# 
# mshs_rad_data <- lapply(list_filenames_rawdata, read_xls)

# Import References -------------------------------------------------------

## Mapping Files -----------------------------------------------------------
map_cc <- read_xlsx(path = paste0(dir_data,
                                  "/References/Radiology RIS Mappings.xlsx"),
                    sheet = "Cost Centers")
map_dpt <- read_xlsx(path = paste0(dir_data,
                                   "/References/Radiology RIS Mappings.xlsx"),
                     sheet = "Departments")
map_mod <- read_xlsx(path = paste0(dir_data,
                                   "/References/Radiology RIS Mappings.xlsx"),
                     sheet = "Select Mod")
map_paycycle <- read_xlsx(path = paste0(dir_universal,
                                        "/Mapping/MSHS_Pay_Cycle.xlsx"))
map_report <- read_xlsx(path = paste0(dir_universal,
                                      "/Mapping",
                                      "/MSHS_Reporting_Definition_Mapping.xlsx"))

## CDMs --------------------------------------------------------------------
import_recent_cdm <- function(dir, site_cdm, file_type) {
  #Compiling Data on Files
  name <- list.files(path = dir, full.names = F, pattern = paste0(file_type, "$"))
  path <- list.files(path = dir, full.names = T, pattern = paste0(file_type, "$"))
  site <- sapply(name, function(x) unlist(str_split(x, pattern = "_"))[1])
  date_created <- sapply(name, function(x)
    unlist(str_split(x, pattern = "_"))[length(unlist(str_split(x, pattern = "_")))])
  type <- sapply(name, function(x) unlist(str_split(x, pattern = "_"))[length(unlist(str_split(x, pattern = "_")))])
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
  if(file_type == "csv"){
    data_import <- read.csv(cdm_file_import$path, sep = ",", header = T,
                            na.strings = c("", "Unavailable", "VOIDV"), fill = T)
  }else if(file_type == "xlsx"){
    data_import <- read_xlsx(cdm_file_import$path, sheet = 1)
  }
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
cat("MSMW CDM File Used:", cdm_file_import$name)
cdm_msbib <- import_recent_cdm(paste0(dir_cdm, "/BIB"), "BI", "xlsx")
cat("MSBIB CDM File Used:", cdm_file_import$name)

# Processing Data ---------------------------------------------------------

## References --------------------------------------------------------------
#Formatting paycycle mapping file to be joined with data frame
  map_paycycle <- map_paycycle %>%
  rename(Date = DATE, `Pay Period End Date` = END.DATE) %>%
  select(Date, `Pay Period End Date`) %>%
  drop_na()

## RIS Data --------------------------------------------------------------------
rad_data_test <- as.data.frame(do.call(rbind, mshs_rad_data)) %>%
  #Combine all charge columns into one column
  pivot_longer(cols = c(`Charge One`, `Charge Two`, `Charge Three`,
                        `Charge Four`, `Charge Five`, `Charge Six`,
                        `Charge Seven`, `Charge Eight`)) %>%
  drop_na(value) %>%
  rename(`Charge Code` = value) %>%
  #Add CPT code, CPT code description, and charge class
  left_join('cdm joining tbd based on site') %>%
  #Add PAT Type and Setting
  left_join(map_dpt) %>%
  mutate(Identifier = paste0(Org, "-",`Charge Class`, "-",Setting)) %>%
  #Add in Cost Center
  left_join(select(map_cc, Identifier, `Dummy Cost Center_Premier`,
                   `Cost Center Description`)) %>%
  rename(`Cost Center` = `Dummy Cost Center_Premier`) %>%
  #Add select modifiers to end of CPT code
  mutate(`CPT Code & Mod` = case_when(
    `Charge Mod 1` %in% map_mod$`Select Modifiers` ~ paste0(`CPT Code`, `Charge Mod 1`),
    `Charge Mod 2` %in% map_mod$`Select Modifiers` ~ paste0(`CPT Code`, `Charge Mod 2`),
    `Charge Mod 3` %in% map_mod$`Select Modifiers` ~ paste0(`CPT Code`, `Charge Mod 3`),
    `Charge Mod 4` %in% map_mod$`Select Modifiers` ~ paste0(`CPT Code`, `Charge Mod 4`),
    TRUE ~ `CPT Code`),
    #Formatting date to remove timestamp
    Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  #Add in Paycycle End Dates
  left_join(map_paycycle)




