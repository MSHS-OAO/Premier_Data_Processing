# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(xlsx)

# Constants ---------------------------------------------------------------
start_date <- as.Date("2022-07-03")
end_date <- as.Date("2022-07-30")
#Defining paths
dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity",
              "/Productivity/Volume - Data")
dir_cdm <- paste0(dir, "/CDMs/MSMW")
dir_files <- paste0(dir, "/MSLW Data/Both Sites Data/Radiology_RIS Data")
dir_universal <- paste0('J:/deans/Presidents/SixSigma/MSHS Productivity',
                        '/Productivity/Universal Data')

# Import Data -------------------------------------------------------------
#Listing all data files in the source data folder
list_filenames_rawdata <- list.files(
  path = paste0(dir_files, "/Source Data"),
  full.names = T)
#Quality Check on # of files. Should be 2 files per time period for MSM & MSW
if (length(list_filenames_rawdata) %% 2 != 0) {
  stop("Unexpected number of files in selected folder")
}

#Importing all data files in the source data folder
rad_data <- lapply(list_filenames_rawdata, read_xls)
#Importing OR diagnostic data
or_data <- lapply(list.files(path = paste0(dir_files, "/OR Diag Source Data"),
                             full.names = T), function(x){read_xls(x, skip = 1)})

#Importing mapping files
map_cc <- read_xlsx(path = paste0(dir_files, "/MSMW_Radiology RIS Mappings.xlsx"),
                    sheet = "Cost Centers")
map_dpt <- read_xlsx(path = paste0(dir_files, "/MSMW_Radiology RIS Mappings.xlsx"),
                    sheet = "Departments")
map_mod <- read_xlsx(path = paste0(dir_files, "/MSMW_Radiology RIS Mappings.xlsx"),
                     sheet = "Select Mod")
map_paycycle <- read_xlsx(path = paste0(dir_universal,
                                        "/Mapping/MSHS_Pay_Cycle.xlsx"))
map_report <- read_xlsx(path = paste0(dir_universal,
                                      "/Mapping",
                                      "/MSHS_Reporting_Definition_Mapping.xlsx"))

#Importing and formatting CDM
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
    select(CHARGE_CODE, OPTB_cpt4, CHARGE_DESC, CHARGE_CLASS) %>%
    rename(`Charge Code` = CHARGE_CODE,
           `CPT Code` = OPTB_cpt4,
           `Charge Class` = CHARGE_CLASS) %>%
    mutate(`Charge Class` = as.numeric(`Charge Class`))
  return(data_export)
}
map_cdm <- import_recent_cdm("SLR")
cat("CDM File Used:", cdm_file_import$name)

# Preprocess Data ---------------------------------------------------------
#Formatting paycycle mapping file to be joined with data frame
map_paycycle <- map_paycycle %>%
  rename(Date = DATE, `Pay Period End Date` = END.DATE) %>%
  select(Date, `Pay Period End Date`) %>%
  drop_na()

#Formatting data frame
rad_data <- as.data.frame(do.call(rbind, rad_data)) %>%
  #Combine all charge columns into one column
  pivot_longer(cols = c(`Charge One`, `Charge Two`, `Charge Three`,
                        `Charge Four`, `Charge Five`, `Charge Six`,
                        `Charge Seven`, `Charge Eight`)) %>%
  drop_na(value) %>%
  rename(`Charge Code` = value) %>%
  #Add CPT code, CPT code description, and charge class
  left_join(map_cdm) %>%
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

#Formatting OR data frame - still needs work
# or_data_test <- lapply(or_data, function(x){
#   colnames(x) <- x[2,]
#   x <- x[3:nrow(x),]
# })


# Creating Premier Upload File --------------------------------------------
upload_file <- rad_data %>%
  #Selecting needed data
  select(Org, `Cost Center`, Date, `CPT Code & Mod`) %>%
  mutate(Volume = 1) %>%
  #Filtering on dates needed for upload
  filter(Date <= end_date, Date >= start_date) %>%
  #Dropping blank CPT codes and unmapped Cost Centers
  drop_na(`Cost Center`,`CPT Code & Mod`) %>%
  #Summing up all the charges by date, cost center, and cpt code
  group_by(Org, `Cost Center`, Date, `CPT Code & Mod`) %>%
  summarise(Volume = sum(Volume)) %>%
  #Creating columns needed
  mutate(Corp = "729805",
         Bud = "0",
         #Formatting site to Premier site code
         `Premier Site` = case_when(
           Org == "RH" ~ "NY2162",
           Org == "SL" ~ "NY2163"),
         `Start Date` = format(as.Date(Date, format = "%Y-%m-%d"),
                               format = "%m/%d/%Y"),
         `End Date` = `Start Date`) %>%
  #Selecting and arranging columns in Premier format
  ungroup() %>%
  select(Corp, `Premier Site`, `Cost Center`,`Start Date`, `End Date`,
         `CPT Code & Mod`, Volume, Bud)

# Creating Quality Chart --------------------------------------------------
quality_chart <- rad_data %>%
  select(Org, `Cost Center`, `Pay Period End Date`) %>%
  mutate(Volume = 1,
         `Pay Period End Date` = format(`Pay Period End Date`, "%d%b%y")) %>%
  pivot_wider(values_from = "Volume",
              values_fn = sum,
              names_from = "Pay Period End Date") %>%
  arrange(Org, `Cost Center`)

# Exporting Files ---------------------------------------------------------
write.table(upload_file,
            file = paste0(dir_files,
                          "/MSMW_Radiology RIS CPT Data_",
                          format(range(as.Date(upload_file$`Start Date`,
                                               format = "%m/%d/%Y"))[1],
                                 "%d%b%y"),
                          " to ",
                          format(range(as.Date(upload_file$`End Date`,
                                               format = "%m/%d/%Y"))[2],
                                 "%d%b%y"),
                          ".csv"),
            sep = ",", row.names = F, col.names = F)

#Exporting RDS data
#TBD

#Exporting Quality Chart
write.xlsx2(quality_chart,
            file = paste0(dir_files,
                          "/MSMW_Quality Chart_Radiology RIS CPT Data _",
                          format(Sys.Date(), format = "%d%b%y"),
                          ".xlsx"),
            sheetName = "MSMW")

