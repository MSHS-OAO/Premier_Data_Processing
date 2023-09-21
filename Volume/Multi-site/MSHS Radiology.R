# Libraries  --------------------------------------------------------------
library(tidyverse)
library(readxl)
library(readxl)
library(openxlsx)
library(lubridate)

# Directories -------------------------------------------------------------
dir_data <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity",
                   "/Productivity/Volume - Data/Multisite Volumes",
                   "/Radiology RIS CPT Data")
dir_cdm <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity",
                  "/Productivity/Volume - Data/CDMs")
dir_universal <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity",
                        "/Productivity/Universal Data")

# Constants ---------------------------------------------------------------
premier_corp <- "729805"
premier_budget <- 0

#Diagnostic OR scaling factor
diagnostic_scaling <- 2.69

# Import References -------------------------------------------------------

## Mapping Files -----------------------------------------------------------
map_premier_sites <- read_xlsx(path = paste0(dir_data, "/References",
                                             "/Radiology RIS Mappings.xlsx"),
                               sheet = "Premier Sites")
map_cost_centers <- read_xlsx(path = paste0(dir_data, "/References",
                                            "/Radiology RIS Mappings.xlsx"),
                              sheet = "Cost Centers")
map_pat_setting <- read_xlsx(path = paste0(dir_data, "/References",
                                           "/Radiology RIS Mappings.xlsx"),
                             sheet = "PAT Setting")
map_charge_class <- read_xlsx(path = paste0(dir_data, "/References",
                                            "/Radiology RIS Mappings.xlsx"),
                              sheet = "Charge Class")
map_cpt_mod <- read_xlsx(path = paste0(dir_data, "/References",
                                       "/Radiology RIS Mappings.xlsx"),
                         sheet = "Select Mod")
map_msbi_special <- read_xlsx(path = paste0(dir_data, "/References",
                                            "/Radiology RIS Mappings.xlsx"),
                              sheet = "MSBI Update Charge Class")
map_paycycle <- read_xlsx(path = paste0(dir_universal,
                                        "/Mapping/MSHS_Pay_Cycle.xlsx"))
map_report <- read_xlsx(path = paste0(dir_universal, "/Mapping",
                                      "/MSHS_Reporting_Definition_Mapping",
                                      ".xlsx"))
map_msh_modality <- read_xlsx(path = paste0(dir_data, "/References",
                                            "/Radiology RIS Mappings.xlsx"),
                              sheet = "Modality Mapping MSH")

map_msq_resource <- read_xlsx(path = paste0(dir_data,"/References",
                                            "/Radiology RIS Mappings.xlsx"),
                              sheet = "Resource Mapping MSQ",
                              col_types = c("text","text","text"))

## CDMs --------------------------------------------------------------------
import_recent_cdm <- function(dir, site_cdm, file_type) {
  #Compiling Data on Files
  name <- list.files(path = dir, full.names = F,
                     pattern = paste0(file_type, "$"))
  path <- list.files(path = dir, full.names = T,
                     pattern = paste0(file_type, "$"))
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
  if (file_type == "csv") {
    data_import <- read.csv(cdm_file_import$path,
                            sep = ",",
                            header = T,
                            fill = T,
                            na.strings = c("", "Unavailable", "VOIDV"))
  }else if (file_type == "xlsx") {
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

#read in CDM
cdm_msq <- read_xlsx(paste0(dir_cdm, "/MSQ/MSQ_CDM_combined.xlsx"),
                     col_types = rep("text",21))

#select columns from CDM for join
CDM_join <- cdm_msq %>%
  select(Code, AlternateCode, description) %>%
  mutate(`Charge Class` = 0) %>%
  distinct()
colnames(CDM_join) <- c("Charge Code", "CPT Code", "CHARGE_DESC", "Charge Class") 

# Import Data -------------------------------------------------------------
#----------------------OR Data-----------------------------------
# Define the function
transform_or_data <- function(file_path, org_value, site_value, cost_center_value) {
  # Get list of files in the directory and sort them by modification time
  file_list <- list.files(path = file_path, pattern = "\\.csv$", full.names = TRUE)
  sorted_files <- file_list[order(file.info(file_list)$mtime, decreasing = TRUE)]
  
  # Read the most recent CSV file
  data <- read.csv(sorted_files[1], skip = 1)
  
  # get days in the month
  month_days <- seq(floor_date(mdy(data$Date[2]), 'month'), 
                    ceiling_date(mdy(data$Date[2]), 'month') - 1, by = 1)
  
  # create data frame for daily upload
  daily_vol <- data.frame(`Cost Center` = rep(cost_center_value,
                                              times = length(month_days)),
                          Start = month_days,
                          End = month_days,
                          `Premier Site` = site_value)
  
  data_filtered <- data %>%
    slice(-1) %>%
    filter(Org %in% org_value) %>%
    mutate(`Premier Site` = site_value,
           `Cost Center` = cost_center_value,
           Start = mdy(Date),
           End = mdy(Date),
           CPT4 = "71045") %>%
    select(`Premier Site`, `Cost Center`, Start, End, CPT4) %>%
    right_join(daily_vol, by = c("Cost Center" = "Cost.Center",
                                 "Start" = "Start",
                                 "End" = "End",
                                 "Premier Site" = "Premier.Site"))
  
  data_transformed <- data_filtered %>%
    mutate(Start = paste0(substr(Start, 6, 7), "/",
                          substr(Start, 9, 10), "/",
                          substr(Start, 1, 4)),
           End = paste0(substr(End, 6, 7), "/",
                        substr(End, 9, 10), "/",
                        substr(End, 1, 4)))
  
  return(data_transformed)
}

msh_rad_or_data <- transform_or_data(file_path = paste0(dir_data, "/OR Source Data"),
                                     org_value = "RM", 
                                     site_value =  "NY0014",
                                     cost_center_value = "MSHRIS21008OR")

msq_rad_or_data <- transform_or_data(file_path = paste0(dir_data, "/OR Source Data")
                                     , org_value = "QN", 
                                     site_value =  "NY0014",
                                     cost_center_value = "MSQRIS21008OR")

msw_rad_or_data <- transform_or_data(file_path = paste0(dir_data, "/OR Source Data")
                                     , org_value = "RH", 
                                     site_value =  "NY2162",
                                     cost_center_value = "MSWRIS21008OR")

msm_rad_or_data <- transform_or_data(file_path = paste0(dir_data, "/OR Source Data")
                                     , org_value = "SL", 
                                     site_value =  "NY2163",
                                     cost_center_value = "MSMRIS21008OR")

msb_rad_or_data <- transform_or_data(file_path = paste0(dir_data, "/OR Source Data")
                                     , org_value = "KH", 
                                     site_value =  "630571",
                                     cost_center_value = "MSBRIS21008OR")

msbi_rad_or_data <- transform_or_data(file_path = paste0(dir_data, "/OR Source Data")
                                      , org_value = "BI", 
                                      site_value =  "630571",
                                      cost_center_value = "MSBIRIS21008OR")
## RIS Data ----------------------------------------------------------------
import_recent_RIS_files <- function(folder.path,
                                    most_recent_files = 1,
                                    sites = "All Sites") {
  #Importing File information from Folder
  File.Name <- list.files(path = folder.path,
                          pattern = "xls$",
                          full.names = F)
  File.Site <- sapply(File.Name, function(x)
    unlist(str_split(x, pattern = " "))[3])
  File.Path <- list.files(path = folder.path,
                          pattern = "xls$",
                          full.names = T)
  File.Date <- as.Date(sapply(File.Name,
                              function(x) paste0("01",
                                                 substr(x, nchar(x) - 9,
                                                        nchar(x) - 4))),
                       format = "%d%m%Y")
  File.Table <- data.table::data.table(File.Name, File.Site, File.Date,
                                       File.Path) %>%
    arrange(desc(File.Date))
  all_dates <- File.Table %>% select(File.Date) %>% unique()
  #File.Table <- File.Table %>% filter(File.Date %in% all_dates[place])
  if (most_recent_files == 0) {
    selected_month <- select.list(choices = format(all_dates, "%B %Y"),
                                  multiple = F,
                                  title = "Select month of data needed",
                                  graphics = T)
  }else{
    selected_month <- format(max(all_dates$File.Date), "%B %Y")
  }
  
  if (sites != "All Sites") {
    user_selected_sites <- select.list(choices = map_premier_sites$`Site Name`,
                                       multiple = T,
                                       title = "Select sites needed",
                                       graphics = T)
    selected_sites <- subset(map_premier_sites,
                             subset = `Site Name` %in% user_selected_sites)$Org
  }else{
    selected_sites <- map_premier_sites$Org
  }
  
  #Quality Check on number of files:
  # if number files selected doesn't match number sites selected
  if (sites == "All Sites") {
    if (nrow(File.Table) %% 11 != 0) {
      stop("Unexpected number of files in selected folder. There are ",
           nrow(File.Table),
           " files for ",
           format(unique(File.Table$File.Date), format = "%B %Y"),
           " and there should be a total of 11 files for each month.")
    }
  }else{
    if (nrow(File.Table) %% length(selected_sites) != 0) {
      stop("Unexpected number of files in selected folder. There are ",
           nrow(File.Table),
           " files for ",
           format(unique(File.Table$File.Date), format = "%B %Y"),
           " and there should be a total of",
           length(selected_sites),
           "files for each month.")
    }
  }
  
  File.Table <<- File.Table
  #Importing Data
  data_files <- File.Table %>%
    filter(File.Date == as.Date(paste("01",
                                      selected_month),
                                format = "%d %B %Y"))
  #Alert if there are missing data files
  if (sites == "All Sites" & any(!data_files$File.Site %in% selected_sites)) {
    missing_data_files <-
      selected_sites[!selected_sites %in% data_files$File.Site]
    missing_data_files <- map_premier_sites %>%
      filter(Org %in% missing_data_files)
    showDialog(title = "Data Files Missing!",
               message = paste("Sites Missing:",
                               paste(missing_data_files$`Site Name`,
                                     collapse = ", ")))
  }
  data_files <- data_files %>% filter(File.Site %in% selected_sites)
  cat("Data files selected for the month",
      format(unique(data_files$File.Date), format = "%B %Y"))
  data_files <- lapply(data_files$File.Path,
                       function(x) read_xls(x,
                                            col_types = c("text",
                                                          "date",
                                                          rep("text", 19))))
  return(data_files)
}
#leave argument sites blank if you want all sites to be selected or put
#"other" to select sites you want
# most_recent_files = 0 will allow you to selct any time period of file
mshs_rad_data <- import_recent_RIS_files(folder.path = paste0(dir_data,
                                                              "/Source Data"), 
                                         most_recent_files = 1,
                                         sites = "other")

# Processing Data ---------------------------------------------------------

## References --------------------------------------------------------------
#Formatting paycycle mapping file to be joined with data frame
map_paycycle <- map_paycycle %>%
  rename(Date = DATE, `Pay Period End Date` = END.DATE) %>%
  select(Date, `Pay Period End Date`) %>%
  drop_na()

map_cost_centers <- map_cost_centers %>%
  filter(`Dummy Cost Center` != "EXCLUDE")

cdm_complete_list <- cdm_msbib %>%
  mutate(`Premier Site` = "630571") %>%
  rbind(cdm_msmsw %>%
          mutate(`Premier Site` = "NY2162"))  %>%
  rbind(cdm_msmsw %>%
          mutate(`Premier Site` = "NY2163")) %>%
  rbind(CDM_join %>%
          mutate(`Premier Site` = "NY0014"))


## RIS Data ------------------------------------------------------------------
mshs_rad_data <- as.data.frame(do.call(rbind, mshs_rad_data)) %>%
  #Combine all charge columns into one column
  pivot_longer(cols = c(`Charge One`, `Charge Two`, `Charge Three`,
                        `Charge Four`, `Charge Five`, `Charge Six`,
                        `Charge Seven`, `Charge Eight`)) %>%
  drop_na(value) %>%
  rename(`Charge Code` = value)

rows_pre_join <- nrow(mshs_rad_data)

mshs_rad_data <- mshs_rad_data %>%
  left_join(map_premier_sites)

#quality check on number of rows from left join
if (rows_pre_join != nrow(mshs_rad_data)) {
  stop("Duplicate rows created! Check Dictionaries for duplicates.")
}

# get days in the month
month_days <- seq.Date(floor_date(as.Date(mshs_rad_data$Date[1]), 'month'), 
                       as.Date(ceiling_date(mshs_rad_data$Date[1], 'month') - 1), by = 1)

#---------------BISLR--------------------------------------------------
bislr_rad_data <- mshs_rad_data %>%
  filter(Org %in% c("RH", "SL", "BI", "BC", "KH", "BH")) %>%
  #Add CPT code, CPT code description, and charge class
  left_join(cdm_complete_list) %>%
  #Updating special MSBIB resource charge class
  mutate(
    `Charge Class` = case_when(
      (Resource %in% map_msbi_special$Resource
       & `Charge Class` %in% map_msbi_special$Current_Charge_Class)
      #Note the mapping file only works if they are being mapped to
      #the same charge_class
      ~ map_msbi_special$Charge_Class[1],
      TRUE ~ `Charge Class`)) %>%
  #Updating Phillips missing cpt codes
  mutate(
    `Charge Class` = case_when(
      (Org == "PH" & substr(`Charge Code`, 1, 2) == "99") ~ 410,
      TRUE ~`Charge Class`),
    `CPT Code` = case_when(
      (Org == "PH" & substr(`Charge Code`, 1, 2) == "99") ~ `Charge Code`,
      TRUE ~ `CPT Code`)) %>%
  #Add PAT Type and Setting
  left_join(map_pat_setting) %>%
  mutate(Identifier = paste0(Org, "-", `Charge Class`, "-", Setting)) %>%
  #Add in Cost Center
  left_join(select(map_cost_centers, Identifier, `Dummy Cost Center`,
                   `Cost Center Description`)) %>%
  rename(`Cost Center` = `Dummy Cost Center`) %>%
  #Add select modifiers to end of CPT code
  mutate(`CPT Code & Mod` = case_when(
    `Charge Mod 1` %in% map_cpt_mod$`Select Modifiers` ~
      paste0(`CPT Code`, `Charge Mod 1`),
    `Charge Mod 2` %in% map_cpt_mod$`Select Modifiers` ~
      paste0(`CPT Code`, `Charge Mod 2`),
    `Charge Mod 3` %in% map_cpt_mod$`Select Modifiers` ~
      paste0(`CPT Code`, `Charge Mod 3`),
    `Charge Mod 4` %in% map_cpt_mod$`Select Modifiers` ~
      paste0(`CPT Code`, `Charge Mod 4`),
    TRUE ~ `CPT Code`),
    #Formatting date to remove timestamp
    Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  #Add in Paycycle End Dates
  left_join(map_paycycle)

#---------------MSH---------------------------------------------------------

#modifiers we want to keep
acceptable_modifiers <- c("26","50","53","tc")

msh_rad_data <- mshs_rad_data %>%
  filter(Org %in% "RM") %>%
  mutate(
    `Charge Mod 1` = case_when(
      `Charge Mod 1` %in% acceptable_modifiers ~ `Charge Mod 1`,
      TRUE ~ ""),
    `Charge Mod 2` = case_when(
      `Charge Mod 2` %in% acceptable_modifiers ~ `Charge Mod 2`,
      TRUE ~ ""),
    `Charge Mod 3` = case_when(
      `Charge Mod 3` %in% acceptable_modifiers ~ `Charge Mod 3`,
      TRUE ~ ""),
    `Charge Mod 4` = case_when(
      `Charge Mod 4` %in% acceptable_modifiers ~ `Charge Mod 4`,
      TRUE ~ "")) %>%
  #unite charge and 
  unite(CPT4,
        `Charge Code`, `Charge Mod 1`, `Charge Mod 2`, `Charge Mod 3`, 
        `Charge Mod 4`, sep = "") %>%
  left_join(map_pat_setting) %>%
  left_join(map_msh_modality) %>%
  mutate(Identifier = paste0(Org,"-",Modality,"-", Setting)) %>%
  left_join(select(map_cost_centers, Identifier, `Dummy Cost Center`,
                   `Cost Center Description`)) %>%
  filter(!is.na(`Dummy Cost Center`)) %>%
  rename(`Cost Center` = `Dummy Cost Center`) %>%
  mutate(`Premier Site` = "NY0014",
         Start = as.Date(Date),
         End = as.Date(Date)) %>%
  select(`Premier Site`, `Cost Center`, Start, End, CPT4)

# create data frame for daily upload
msh_daily_vol <- data.frame(`Cost Center` = rep(unique(msh_rad_data$`Cost Center`),
                                            times = length(month_days))) %>%
  arrange(Cost.Center) %>%
  mutate(Start = rep(month_days, length(unique(msh_rad_data$`Cost Center`))),
         End = rep(month_days, length(unique(msh_rad_data$`Cost Center`))),
         `Premier Site` = rep("NY0014", length(rep(unique(msh_rad_data$`Cost Center`),
                                                   times = length(month_days)))))

# ensure each cost center has a row for each day
msh_rad_data <- msh_rad_data %>%
  right_join(msh_daily_vol, by = c("Cost Center" = "Cost.Center",
                               "Start" = "Start",
                               "End" = "End",
                               "Premier Site" = "Premier Site")) %>%
  mutate(Start = paste0(substr(Start,6,7), "/",
                        substr(Start,9,10), "/",
                        substr(Start,1,4)),
         End = paste0(substr(End,6,7), "/",
                      substr(End,9,10), "/",
                      substr(End,1,4)))

#--------------Neuro---------------------------------------------
neuro_rad_data <- mshs_rad_data %>%
  filter(Org %in% "NS") %>%
  #remove unwanted modifier
  mutate(
    `Charge Mod 1` = case_when(
      `Charge Mod 1` %in% acceptable_modifiers ~ `Charge Mod 1`,
      TRUE ~ ""),
    `Charge Mod 2` = case_when(
      `Charge Mod 2` %in% acceptable_modifiers ~ `Charge Mod 2`,
      TRUE ~ ""),
    `Charge Mod 3` = case_when(
      `Charge Mod 3` %in% acceptable_modifiers ~ `Charge Mod 3`,
      TRUE ~ ""),
    `Charge Mod 4` = case_when(
      `Charge Mod 4` %in% acceptable_modifiers ~ `Charge Mod 4`,
      TRUE ~ "")) %>%
  #unite charge and 
  unite(CPT4, 
        `Charge Code`, `Charge Mod 1`, `Charge Mod 2`, `Charge Mod 3`, 
        `Charge Mod 4`, sep = "") %>%
  #Add in necessary columns for upload
  mutate(`Premier Site` = "NY0014",
         `Cost Center` = "MSHRIS21050",
         Start = as.Date(Date),
         End = as.Date(Date)) %>%
  #select column order for upload
  select(`Premier Site`, `Cost Center`, Start, End, CPT4)

# create data frame for daily upload
neuro_daily_vol <- data.frame(`Cost Center` = rep(unique(neuro_rad_data$`Cost Center`),
                                                times = length(month_days))) %>%
  arrange(Cost.Center) %>%
  mutate(Start = rep(month_days, length(unique(neuro_rad_data$`Cost Center`))),
         End = rep(month_days, length(unique(neuro_rad_data$`Cost Center`))),
         `Premier Site` = rep("NY0014", length(rep(unique(neuro_rad_data$`Cost Center`),
                                                   times = length(month_days)))))
# ensure each cost center has a row for each day
neuro_rad_data <- neuro_rad_data %>%
  right_join(neuro_daily_vol, by = c("Cost Center" = "Cost.Center",
                                   "Start" = "Start",
                                   "End" = "End",
                                   "Premier Site" = "Premier Site")) %>%
  mutate(Start = paste0(substr(Start,6,7), "/",
                        substr(Start,9,10), "/",
                        substr(Start,1,4)),
         End = paste0(substr(End,6,7), "/",
                      substr(End,9,10), "/",
                      substr(End,1,4)))

#bind both files for upload
MSH_upload <- rbind(msh_rad_data, neuro_rad_data, msh_rad_or_data) %>%
  group_by(`Premier Site`, `Cost Center`, Start, End, CPT4) %>%
  summarise(volume = n()) %>%
  mutate(volume = case_when(
    is.na(CPT4) ~ 0,
    TRUE ~ volume)) %>%
  mutate(CPT4 = replace_na(CPT4, "71045")) %>%
  mutate(Partner = "729805",
         Budget = premier_budget,
         volume = as.numeric(volume)) %>%
  select(Partner, `Premier Site`, `Cost Center`, Start, End, CPT4, volume, 
         Budget)
MSH_upload <- MSH_upload %>%
  mutate(volume = case_when(
    `Cost Center` == "MSHRIS21008OR" ~ volume * diagnostic_scaling,
    TRUE ~ volume))

#------------------MSQ--------------------------------------------------
msq_rad_data <- mshs_rad_data %>%
  filter(Org %in% "QN") %>%
  #remove unwanted modifiers and concatenate remaining
  mutate(
    `Charge Mod 1` = case_when(
      `Charge Mod 1` %in% acceptable_modifiers ~ `Charge Mod 1`,
      TRUE ~ ""),
    `Charge Mod 2` = case_when(
      `Charge Mod 2` %in% acceptable_modifiers ~ `Charge Mod 2`,
      TRUE ~ ""),
    `Charge Mod 3` = case_when(
      `Charge Mod 3` %in% acceptable_modifiers ~ `Charge Mod 3`,
      TRUE ~ ""),
    `Charge Mod 4` = case_when(
      `Charge Mod 4` %in% acceptable_modifiers ~ `Charge Mod 4`,
      TRUE ~ ""),
    Modifier = paste0(`Charge Mod 1`,`Charge Mod 2`,
                      `Charge Mod 3`,`Charge Mod 4`))

#join charge detail with CDM, PAT, and DEP IT
msq_rad_data <- left_join(msq_rad_data, CDM_join) %>%
  filter(!is.na(`CPT Code`)) %>%
  left_join(map_pat_setting) %>%
  left_join(map_msq_resource) %>%
  mutate(Identifier = paste0(Org,"-",Dept,"-", Setting)) %>%
  mutate(CPT4 = paste0(`CPT Code` ,Modifier)) %>%
  left_join(map_cost_centers) %>%
  filter(!is.na(`Dummy Cost Center`)) %>%
  mutate(Start = as.Date(Date),
         End = as.Date(Date)) %>%
  select(`Premier Site`, `Dummy Cost Center`, Start, End, CPT4) %>%
  rename(`Cost Center` = `Dummy Cost Center`)

# create data frame for daily upload
msq_daily_vol <- data.frame(`Cost Center` = rep(unique(msq_rad_data$`Cost Center`),
                                                  times = length(month_days))) %>%
  arrange(Cost.Center) %>%
  mutate(Start = rep(month_days, length(unique(msq_rad_data$`Cost Center`))),
         End = rep(month_days, length(unique(msq_rad_data$`Cost Center`))),
         `Premier Site` = rep("NY0014", length(rep(unique(msq_rad_data$`Cost Center`),
                                                   times = length(month_days)))))

# ensure each cost center has a row for each day
msq_rad_data <- msq_rad_data %>%
  right_join(msq_daily_vol, by = c("Cost Center" = "Cost.Center",
                                     "Start" = "Start",
                                     "End" = "End",
                                     "Premier Site" = "Premier Site")) %>%
  mutate(Start = paste0(substr(Start,6,7), "/",
                        substr(Start,9,10), "/",
                        substr(Start,1,4)),
         End = paste0(substr(End,6,7), "/",
                      substr(End,9,10), "/",
                      substr(End,1,4)))

#combine upload files
MSQ_upload <- rbind(msq_rad_data, msq_rad_or_data) %>%
  group_by(`Premier Site`, `Cost Center`, Start, End, CPT4) %>%
  summarise(Volume = n()) %>%
  mutate(Volume = case_when(
    is.na(CPT4) ~ 0,
    TRUE ~ Volume)) %>%
  mutate(CPT4 = replace_na(CPT4, "71045")) %>%
  mutate(Partner = "729805",
         Budget = premier_budget) %>%
  select(Partner, `Premier Site`, `Cost Center`, Start, End, CPT4, Volume, 
         Budget)

# Creating Outputs --------------------------------------------------------

## Premier Upload Files ----------------------------------------------------
create_premier_upload <- function(df, selected_sites) {
  premier_daily_vol <- df %>%
    filter(!is.na(`Cost Center`),
           `Premier Site` %in% selected_sites) %>%
    select(`Cost Center`, `Premier Site`) %>%
    distinct()
  
  premier_daily_vol <- data.frame(`Cost Center` = rep(premier_daily_vol$`Cost Center`, 
                                                      times = length(month_days)),
                                  `Premier Site` = rep(premier_daily_vol$`Premier Site`, 
                                                       times = length(month_days))) %>%
    arrange(Cost.Center) %>%
    mutate(Start = rep(month_days, length(unique(premier_daily_vol$`Cost Center`))),
           End = rep(month_days, length(unique(premier_daily_vol$`Cost Center`))),
           Corp = rep(premier_corp, nrow(premier_daily_vol) * length(month_days)))
  return_df <- df %>%
    select(`Premier Site`, `Cost Center`, Date, `CPT Code & Mod`) %>%
    mutate(Volume = 1) %>%
    #Filtering on dates needed for upload
    filter(`Premier Site` %in% selected_sites) %>%
    #Dropping blank CPT codes and unmapped Cost Centers
    drop_na(`Cost Center`, `CPT Code & Mod`) %>%
    #Summing up all the charges by date, cost center, and cpt code
    group_by(`Premier Site`, `Cost Center`, Date, `CPT Code & Mod`) %>%
    summarise(Volume = sum(Volume)) %>%
    #Creating columns needed
    mutate(Corp = premier_corp,
           Start = as.Date(Date),
           End = as.Date(Date)) %>%
    #Selecting and arranging columns in Premier format
    ungroup() %>%
    select(Corp, `Premier Site`, `Cost Center`, Start, End,
           `CPT Code & Mod`, Volume)
  
  return_df <- return_df %>%
    right_join(premier_daily_vol, by = c("Cost Center" = "Cost.Center",
                                         "Start" = "Start",
                                         "End" = "End",
                                         "Premier Site" = "Premier.Site",
                                         "Corp" = "Corp")) %>%
    mutate(Start = paste0(substr(Start,6,7), "/",
                          substr(Start,9,10), "/",
                          substr(Start,1,4)),
           End = paste0(substr(End,6,7), "/",
                        substr(End,9,10), "/",
                        substr(End,1,4))) %>%
    mutate(Volume = case_when(
                          is.na(`CPT Code & Mod`) ~ 0,
                          TRUE ~ Volume)) %>%
    mutate(`CPT Code & Mod` = replace_na(`CPT Code & Mod`, "71045"),
           Bud = premier_budget)
}

msmw_upload <- create_premier_upload(df = bislr_rad_data,
                                     selected_sites = c("NY2162", "NY2163"))
msbib_upload <- create_premier_upload(df = bislr_rad_data,
                                      selected_sites = c("630571"))

bislr_or_upload <- rbind(msm_rad_or_data, msw_rad_or_data, msb_rad_or_data, 
                         msbi_rad_or_data) %>%
  group_by(`Premier Site`, `Cost Center`, Start, End, CPT4) %>%
  summarise(Volume = n()) %>%
  mutate(Volume = case_when(
    is.na(CPT4) ~ 0,
    TRUE ~ Volume)) %>%
  mutate(CPT4 = replace_na(CPT4, "71045")) %>%
  mutate(Partner = "729805",
         Budget = premier_budget) %>%
  select(Partner, `Premier Site`, `Cost Center`, Start, End, CPT4, Volume, 
         Budget)
## Quality Charts ----------------------------------------------------------
quality_chart <- rbind(msmw_upload, msbib_upload)

# Outputs -----------------------------------------------------------------
write.table(msmw_upload,
            file = paste0(dir_data,
                          "/Upload Files",
                          "/MSMW RIS CPT_",
                          format(as.Date(min(msmw_upload$End),
                                         format = "%m/%d/%Y"),
                                 "%Y-%m-%d"),
                          " to ",
                          format(as.Date(max(msmw_upload$End),
                                         format = "%m/%d/%Y"),
                                 "%Y-%m-%d"),
                          ".csv"),
            sep = ",",
            row.names = F,
            col.names = F)
write.table(msbib_upload,
            file = paste0(dir_data,
                          "/Upload Files",
                          "/MSBIB RIS CPT_",
                          format(as.Date(min(msbib_upload$End),
                                         format = "%m/%d/%Y"),
                                 "%Y-%m-%d"),
                          " to ",
                          format(as.Date(max(msbib_upload$End),
                                         format = "%m/%d/%Y"),
                                 "%Y-%m-%d"),
                          ".csv"),
            sep = ",",
            row.names = F,
            col.names = F)

write.table(bislr_or_upload, file = paste0(dir_data,
                                           "/Upload Files/OR Uploads",
                                           "/BISLR RIS OR CPT_",
                                           format(as.Date(min(bislr_or_upload$End),
                                                          format = "%m/%d/%Y"),
                                                  "%Y-%m-%d"),
                                           " to ",
                                           format(as.Date(max(bislr_or_upload$End),
                                                          format = "%m/%d/%Y"),
                                                  "%Y-%m-%d"),
                                           ".csv"),
            sep = ",",
            row.names = F,
            col.names = F)

write.table(MSH_upload, file = paste0(dir_data,
                                      "/Upload Files",
                                      "/MSH RIS CPT_",
                                      format(as.Date(min(MSH_upload$End),
                                                     format = "%m/%d/%Y"),
                                             "%Y-%m-%d"),
                                      " to ",
                                      format(as.Date(max(MSH_upload$End),
                                                     format = "%m/%d/%Y"),
                                             "%Y-%m-%d"),
                                      ".csv"),
            sep = ",",
            row.names = F,
            col.names = F)

write.table(MSQ_upload, file = paste0(dir_data,
                                      "/Upload Files",
                                      "/MSQ RIS CPT_",
                                      format(as.Date(min(MSQ_upload$End),
                                                     format = "%m/%d/%Y"),
                                             "%Y-%m-%d"),
                                      " to ",
                                      format(as.Date(max(MSQ_upload$End),
                                                     format = "%m/%d/%Y"),
                                             "%Y-%m-%d"),
                                      ".csv"),
            sep = ",",
            row.names = F,
            col.names = F)