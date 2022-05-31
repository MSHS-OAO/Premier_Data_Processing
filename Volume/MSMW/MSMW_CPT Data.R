
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(splitstackshape)
library(stringi)
library(xlsx)

# Constants ---------------------------------------------------------------
start_date <- as.Date('2022-02-27')
end_date <- as.Date('2022-03-26')
dir_files <- paste0('J:/deans/Presidents/SixSigma/MSHS Productivity',
                    '/Productivity/Volume - Data/MSLW Data/Both Sites Data',
                    '/Charge Detail/Source Data')
dir_cdm <- paste0('J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity',
                  '/Volume - Data/CDMs/MSMW')
dir_dictionary <- paste0('J:/deans/Presidents/SixSigma/MSHS Productivity',
                         '/Productivity/Volume - Data/MSLW Data',
                         '/Both Sites Data/Charge Detail/Dictionaries',
                         '/MSLW_Revenue to Cost Center Map.xlsx')
dir_export <- paste0('J:/deans/Presidents/SixSigma/MSHS Productivity',
                     '/Productivity/Volume - Data/MSLW Data/Both Sites Data',
                     '/Charge Detail')

# Import Data -------------------------------------------------------------
list_filenames_rawdata <- list.files(
  path = choose.dir(caption = "Select most recent folder",
                    default = dir_files),
  full.names = T,
  pattern = "csv$") # only pulls in .csv files
if (length(list_filenames_rawdata) %% 3 != 0) {
  stop("Unexpected number of files in selected folder")
  }#QC
cpt_data <- lapply(list_filenames_rawdata, read.csv)

# Preprocess Data ---------------------------------------------------------
cpt_data <- lapply(cpt_data, function(x)
  cSplit(x, colnames(x), sep = "|", type.convert = T))
  # separating csvs into tables
col_names_split <- function(df) {
  new_col_names <- unlist(stri_split_fixed(str = colnames(df)[1], "."))
  new_col_names <- sapply(new_col_names,
                         function(x) stri_replace_all(str = x, regex = "_01",
                                                      replacement = ""))
  colnames(df) <- new_col_names
  return(df)
}
cpt_data <- lapply(cpt_data, function(x) col_names_split(x))
cpt_data <- do.call(plyr::rbind.fill, cpt_data)
cpt_data <- cpt_data %>%
  mutate(PostingDate = as.Date(PostingDate),
         ServiceDate = as.Date(ServiceDate),
         FacilityId = as.character(FacilityId),
         RevenueCenter = as.character(RevenueCenter),
         ChargeCode = as.character(ChargeCode))

# Import Dictionaries -----------------------------------------------------
dictionary_cc <- read.xlsx2(file = dir_dictionary, sheetIndex = 1)
dictionary_cc <- dictionary_cc %>%
  select(Premier.Facility.ID, FacilityId, RevenueCenter, Cost.Center,
         Cost.Center.Description) %>%
  mutate(Premier.Facility.ID = as.character(Premier.Facility.ID),
         FacilityId = as.character(FacilityId),
         RevenueCenter = as.character(RevenueCenter),
         Cost.Center = as.character(Cost.Center))
import_recent_cdm <- function(site_cdm) {
  #Compiling Data on Files
  name <- list.files(path = dir_cdm, full.names = F, pattern = "csv$")
  path <- list.files(path = dir_cdm, full.names = T, pattern = "csv$")
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
    select(CHARGE_CODE, OPTB_cpt4, CHARGE_DESC) %>%
    rename(ChargeCode = CHARGE_CODE,
           CPTCode = OPTB_cpt4)
  return(data_export)
}
dictionary_cdm <- import_recent_cdm("SLR")
dictionary_cdm <- dictionary_cdm %>%
  mutate(ChargeCode = as.character(ChargeCode),
         CPTCode = as.character(CPTCode)) %>%
  drop_na()

# Formatting Data ---------------------------------------------------------
cpt_data_upload <- left_join(cpt_data, dictionary_cc)
cpt_data_upload <- left_join(cpt_data_upload, dictionary_cdm)

# Creating Upload File ----------------------------------------------------
file_upload <- function(cpt_data, site, start_date_, end_date_) {
  data_upload <- cpt_data %>%
    filter(ServiceDate >= start_date_,
           ServiceDate <= end_date_,
           Premier.Facility.ID == site) %>%
    mutate(Corp = "729805",
           EndDate = ServiceDate,
           ServiceDate = format(ServiceDate, "%m/%d/%Y"),
           EndDate = format(EndDate, "%m/%d/%Y")) %>%
    select(Corp, Premier.Facility.ID, Cost.Center, ServiceDate, EndDate,
           CPTCode, NumberOfUnits) %>%
    group_by(Corp, Premier.Facility.ID, Cost.Center, ServiceDate, EndDate,
             CPTCode) %>%
    summarise(Volume = sum(NumberOfUnits, na.rm = T)) %>%
    mutate(Budget = 0) %>%
    drop_na()
}
msw_upload <- file_upload(cpt_data_upload, "NY2162", start_date, end_date)
msm_upload <- file_upload(cpt_data_upload, "NY2163", start_date, end_date)
msmw_upload <- rbind(msw_upload, msm_upload)

# Quality Chart -----------------------------------------------------------
cpt_data_upload$Cost.Center.Description <- forcats::fct_explicit_na(
  cpt_data_upload$Cost.Center.Description, na_level = "Other")
quality_chart <- cpt_data_upload %>%
  ungroup() %>%
  select(Premier.Facility.ID, Cost.Center, Cost.Center.Description, ServiceDate,
         NumberOfUnits, PostingDate) %>%
  filter(ServiceDate >= range(PostingDate)[1]) %>%
  mutate(PostingDate = NULL) %>%
  group_by(Premier.Facility.ID, Cost.Center, Cost.Center.Description,
           ServiceDate) %>%
  summarise(TotalCharges = sum(NumberOfUnits)) %>%
  arrange(ServiceDate) %>%
  mutate(ServiceDate = format(ServiceDate, "%b-%Y")) %>%
  pivot_wider(names_from = ServiceDate, values_from = TotalCharges,
              values_fn = list(TotalCharges = sum)) %>%
  arrange(Premier.Facility.ID)

# Exporting Files ---------------------------------------------------------
# write.table(msw_upload,
#             file = paste0(dir_export,
#                           "/MSW_CPT_",
#                           format(as.Date(range(msw_upload$ServiceDate)[1],
#                                          format = "%m/%d/%Y"), "%d%b%y"),
#                           " to ",
#                           format(as.Date(range(msw_upload$ServiceDate)[2],
#                                          format = "%m/%d/%Y"), "%d%b%y"),
#                           ".csv"),
#             sep = ",", row.names = F, col.names = F)
# write.table(msm_upload,
#             file = paste0(dir_export,
#                           "/MSM_CPT_",
#                           format(as.Date(range(msm_upload$ServiceDate)[1],
#                                          format = "%m/%d/%Y"), "%d%b%y"),
#                           " to ",
#                           format(as.Date(range(msm_upload$ServiceDate)[2],
#                                          format = "%m/%d/%Y"), "%d%b%y"),
#                           ".csv"),
#             sep = ",", row.names = F, col.names = F)
write.table(msmw_upload,
            file = paste0(dir_export,
                          "/MSMW_CPT_",
                          format(as.Date(range(msm_upload$ServiceDate)[1],
                                         format = "%m/%d/%Y"), "%d%b%y"),
                          " to ",
                          format(as.Date(range(msm_upload$ServiceDate)[2],
                                         format = "%m/%d/%Y"), "%d%b%y"),
                          ".csv"),
            sep = ",", row.names = F, col.names = F)
write.table(quality_chart,
            file = paste0(dir_export,
                          "/Quality Chart_",
                          format(as.Date(range(cpt_data_upload$PostingDate)[1],
                                         format = "%m/%d/%y"), "%d%b%y"),
                          " to ",
                          format(as.Date(range(cpt_data_upload$PostingDate)[2],
                                         format = "%m/%d/%y"), "%d%b%y"),
                          ".csv"),
            sep = ",", row.names = F, col.names = T)
write_rds(cpt_data_upload, paste0(dir_files,
                                  "/Master_",
                                  format(range(cpt_data_upload$PostingDate)[1],
                                         "%d%b%y"),
                                  " to ",
                                  format(range(cpt_data_upload$PostingDate)[2],
                                         "%d%b%y"),
                                  ".RDS"))
