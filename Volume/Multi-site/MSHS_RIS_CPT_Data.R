
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
  #Dates needed for Premier Upload
  selected_start_date <- as.Date('2022-07-03')
  selected_end_date <- as.Date('2022-07-30')
  
  premier_corp <- '729805'
  premier_budget <- 0
  
  diagnostic_scaling <- 2.69
  
  # Import Data -------------------------------------------------------------
  
  ## OR Data -------------------------------------------------------------
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

# Import References -------------------------------------------------------

  ## Mapping Files -----------------------------------------------------------
  map_premier_sites <-read_xlsx(path = paste0(dir_data,
                                              "/References/Radiology RIS Mappings.xlsx"),
                                sheet = "Premier Sites")
  map_cost_centers <- read_xlsx(path = paste0(dir_data,
                                    "/References/Radiology RIS Mappings.xlsx"),
                      sheet = "Cost Centers")
  map_pat_setting <- read_xlsx(path = paste0(dir_data,
                                     "/References/Radiology RIS Mappings.xlsx"),
                       sheet = "PAT Setting")
  map_charge_class <- read_xlsx(path = paste0(dir_data,
                                     "/References/Radiology RIS Mappings.xlsx"),
                       sheet = "Charge Class")
  map_cpt_mod <- read_xlsx(path = paste0(dir_data,
                                     "/References/Radiology RIS Mappings.xlsx"),
                       sheet = "Select Mod")
  map_msbi_special <- read_xlsx(path = paste0(dir_data,
                                         "/References/Radiology RIS Mappings.xlsx"),
                           sheet = "MSBI Update Charge Class")
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
      data_import <- read.csv(cdm_file_import$path,
                              sep = ",",
                              header = T,
                              fill = T,
                              na.strings = c("", "Unavailable", "VOIDV"))
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
    
  cdm_complete_list <- cdm_msbib %>% 
    mutate(`Premier Site` = '630571') %>%
    rbind(cdm_msmsw %>%
            mutate(`Premier Site` = 'NY2162'))  %>%
    rbind(cdm_msmsw %>%
            mutate(`Premier Site` = 'NY2163'))
  

  ## RIS Data --------------------------------------------------------------------
  mshs_rad_data <- as.data.frame(do.call(rbind, mshs_rad_data)) %>%
    #Combine all charge columns into one column
    pivot_longer(cols = c(`Charge One`, `Charge Two`, `Charge Three`,
                          `Charge Four`, `Charge Five`, `Charge Six`,
                          `Charge Seven`, `Charge Eight`)) %>%
    drop_na(value) %>%
    rename(`Charge Code` = value)
  
  rows_pre_join <- nrow(mshs_rad_data)
  
  mshs_rad_data <- mshs_rad_data %>%
    left_join(map_premier_sites) %>%
    #Add CPT code, CPT code description, and charge class
    left_join(cdm_complete_list) %>%
    #Updating special MSBIB resource charge class
    mutate(
      `Charge Class` = case_when(
        (Resource %in% map_msbi_special$Resource
         & `Charge Class` %in% map_msbi_special$Current_Charge_Class)
        #Note the mapping file only works if they are being mapped to the same charge_class
        ~ map_msbi_special$Charge_Class[1],
        TRUE ~ `Charge Class`)) %>%
    #Updating Phillips missing cpt codes
    mutate(
      `Charge Class` = case_when(
        (Org == 'PH' & substr(`Charge Code`, 1, 2) == "99") ~ 410,
        TRUE ~`Charge Class`),
      `CPT Code` = case_when(
        (Org == 'PH' & substr(`Charge Code`, 1, 2) == "99") ~ `Charge Code`,
        TRUE ~ `CPT Code`)) %>%
    #Add PAT Type and Setting
    left_join(map_pat_setting) %>%
    mutate(Identifier = paste0(Org, "-",`Charge Class`, "-", Setting)) %>%
    #Add in Cost Center
    left_join(select(map_cost_centers, Identifier, `Dummy Cost Center`,
                     `Cost Center Description`)) %>%
    rename(`Cost Center` = `Dummy Cost Center`) %>%
    #Add select modifiers to end of CPT code
    mutate(`CPT Code & Mod` = case_when(
      `Charge Mod 1` %in% map_cpt_mod$`Select Modifiers` ~ paste0(`CPT Code`, `Charge Mod 1`),
      `Charge Mod 2` %in% map_cpt_mod$`Select Modifiers` ~ paste0(`CPT Code`, `Charge Mod 2`),
      `Charge Mod 3` %in% map_cpt_mod$`Select Modifiers` ~ paste0(`CPT Code`, `Charge Mod 3`),
      `Charge Mod 4` %in% map_cpt_mod$`Select Modifiers` ~ paste0(`CPT Code`, `Charge Mod 4`),
      TRUE ~ `CPT Code`),
      #Formatting date to remove timestamp
      Date = as.Date(Date, format = "%Y-%m-%d")) %>%
    #Add in Paycycle End Dates
    left_join(map_paycycle)
  
  #quality check on number of rows from left join
  if(rows_pre_join != nrow(mshs_rad_data)){
    stop('Duplicate rows created! Check Dictionaries for duplicates.')}

# Creating Outputs --------------------------------------------------------

  ## Premier Upload Files ----------------------------------------------------
  create_premier_upload <- function(df, selected_sites, start_date, end_date){
    return_df <- df %>% 
      select(`Premier Site`, `Cost Center`, Date, `CPT Code & Mod`) %>%
      mutate(Volume = 1) %>%
      #Filtering on dates needed for upload
      filter(`Premier Site`%in% selected_sites,
             Date <= end_date,
             Date >= start_date) %>%
      #Dropping blank CPT codes and unmapped Cost Centers
      drop_na(`Cost Center`,`CPT Code & Mod`) %>%
      #Summing up all the charges by date, cost center, and cpt code
      group_by(`Premier Site`, `Cost Center`, Date, `CPT Code & Mod`) %>%
      summarise(Volume = sum(Volume)) %>%
      #Creating columns needed
      mutate(Corp = premier_corp,
             Bud = premier_budget,
             `Start Date` = format(as.Date(Date, format = "%Y-%m-%d"),
                                   format = "%m/%d/%Y"),
             `End Date` = `Start Date`) %>%
      #Selecting and arranging columns in Premier format
      ungroup() %>%
      select(Corp, `Premier Site`, `Cost Center`,`Start Date`, `End Date`,
             `CPT Code & Mod`, Volume, Bud)
  }
  
  msmw_upload <- create_premier_upload(df = mshs_rad_data,
                                       selected_sites = c('NY2162', 'NY2163'),
                                       start_date = selected_start_date,
                                       end_date = selected_end_date)
  msbib_upload <- create_premier_upload(df = mshs_rad_data,
                                       selected_sites = c('630571'),
                                       start_date = selected_start_date,
                                       end_date = selected_end_date)
  
  ## Quality Charts ----------------------------------------------------------

# Outputs -----------------------------------------------------------------


