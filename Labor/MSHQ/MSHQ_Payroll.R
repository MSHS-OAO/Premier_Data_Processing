# MSHQ Payroll ----------------------------------------------------------------

## Libraries & Constants ------------------------------------------------------
library(tidyr)
library(dplyr)
library(DBI)
library(odbc)
library(writexl)
library(lubridate)

data_dir <- paste0("/SharedDrive/deans/Presidents/SixSigma/MSHS Productivity/",
                   "Productivity/")

# list of dept IDs where fund number should be used in place of the
# standard department ID structure
cc_fundnum_conv <- c("77061")
wd_fundnum_conv <- c("201210310113155", "201210345113155")

## Read Labor -----------------------------------------------------------------
df <- file.info(list.files(paste0(data_dir, "Universal Data/Labor/Raw Data/",
                                  "MSHQ_sftp_sync/Insert/")
                           , full.names = T))
df <- read.csv(rownames(df)[which.max(df$mtime)], header = T, sep = "~",
               stringsAsFactors = F, colClasses = rep("character", 33)) %>%
  mutate(JOBCODE = case_when(
    is.na(JOBCODE) ~ "UNKNOWN",
    TRUE ~ JOBCODE))

## Read Mapping Files ---------------------------------------------------------
oao_con <- dbConnect(odbc(), "OAO Cloud DB Production")

mapping_paycode <- tbl(oao_con, "LPM_MAPPING_PAYCODE") %>%
  collect()
mapping_jobcode <- tbl(oao_con, "LPM_MAPPING_JOBCODE") %>%
  filter(PAYROLL == "MSHQ") %>%
  collect()

## Read In Cost Center Dictionary ---------------------------------------------
# download cost center dictionary and current mapped job codes
cost_center_export <- read.csv(paste0(data_dir, "Labor - Data/MSH/Payroll/",
                                      "MSH Labor/2.0/",
                                      "cost_center_export/",
                                      "cost_center_export.csv"))
jobcode_mapping_export <- read.csv(paste0(data_dir, "Labor - Data/MSH/Payroll/",
                                          "MSH Labor/2.0/",
                                          "jobcode_mapping_export/",
                                          "jobcode_mapping_export.csv"),
                                   colClasses = rep("character", 10))

## Mapping Checks -------------------------------------------------------------
### Paycode -------------------------------------------------------------------
# check for new paycodes
new_paycodes <- df %>%
  filter(!(PAYCODE %in% (mapping_paycode %>% select(PAYCODE_RAW) %>% pull()))) %>%
  select(PAYCODE) %>%
  distinct()

if (nrow(new_paycodes) > 0) {
  warning("There are new paycodes in this payroll file")
  new_paycodes
} else {
  print("There are no new paycodes in this payroll file")
}

### Jobcode -------------------------------------------------------------------
# check for new jobcodes in db
new_jobcodes_db <- df %>%
  filter(!(JOBCODE %in% mapping_jobcode$JOBCODE)) %>%
  select(JOBCODE, POSITION_CODE_DESCRIPTION) %>%
  distinct()

# if new jobcodes save file and let user know of any codes with multiple desc
if (nrow(new_jobcodes_db) > 0) {
  warning("There are new jobcodes in this payroll file")
  new_jobcodes_insert <- new_jobcodes_db %>%
    mutate(PAYROLL = "MSHQ",
           PROVIDER = "",
           JOBCODE_PREMIER = "",
           JOBCODE_PREMIER_DESCRIPTION = "") %>%
    rename(JOBCODE_DESCRIPTION = POSITION_CODE_DESCRIPTION) %>%
    select(JOBCODE, PAYROLL, JOBCODE_DESCRIPTION, PROVIDER, JOBCODE_PREMIER, 
           JOBCODE_PREMIER_DESCRIPTION) 
  
  # check if new jobcodes have unique descriptions
  if (length(unique(new_jobcodes_db$JOBCODE)) == nrow(new_jobcodes_db)) {
    print("All new jobcodes have a unique description")
  } else {
    print("A new jobcode has multiple descriptions")
  }
  
  # save the new jobcodes
  write_xlsx(new_jobcodes_db, paste0(data_dir, "Universal Data/Mapping/",
                                     "sftp_sync_decrypt_insert/JobCode/MSHQ/",
                                     "new_mshq_jobcodes_payroll_",
                                     Sys.Date(), ".xlsx"))
  
  print(paste0("The new jobcodes have been saved to ", data_dir, 
               "Universal Data/Mapping/sftp_sync_decrypt_insert/JobCode/MSHQ/"))
  print(paste("User should add the new jobcode mappings to the DB first and",
              "then continue running the code"))
} else {
  print("There are no new jobcodes to be added to the DB")
}

#### Jobcode Dictionary -----------------------------------------------------
# reload the jobcode mapping table from DB
mapping_jobcode <- tbl(oao_con, "LPM_MAPPING_JOBCODE") %>%
  filter(PAYROLL == "MSHQ") %>%
  collect()

# get jobcodes that are not providers and not in premier
new_jobcodes_premier <- df %>%
  left_join(mapping_jobcode, by = c("JOBCODE" = "JOBCODE")) %>%
  filter(PROVIDER == 0) %>%
  mutate(JOBCODE = substr(JOBCODE, 1, 10)) %>%
  filter(!(JOBCODE %in% jobcode_mapping_export$Entity.Job.Code)) %>%
  select(JOBCODE, POSITION_CODE_DESCRIPTION) %>%
  distinct()

# if there are new jobcodes then create and save the dictionary
if (nrow(new_jobcodes_premier) > 0) {
  jobcode_dictionary <- new_jobcodes_premier %>%
    mutate(POSITION_CODE_DESCRIPTION = substr(POSITION_CODE_DESCRIPTION, 1, 50),
           `Corporation Code` = "729805",
           `Entity Code` = "NY0014",
           `Default Agency Hourly Rate` = "",
           `Effective Start Date` = "",
           `Expiration Date` = "") %>%
    rename(`Job Code`= JOBCODE,
           `Job Code Name` = POSITION_CODE_DESCRIPTION) %>%
    select(`Corporation Code`, `Entity Code`, `Job Code`, `Job Code Name`,
           `Default Agency Hourly Rate`, `Effective Start Date`, 
           `Expiration Date`)
  
  # save jobcode dictionary
  write.csv(jobcode_dictionary, paste0(data_dir, "Labor - Data/MSH/Payroll/",
                                       "MSH Labor/2.0/processed_files/",
                                       "jobcode_dictionary/",
                                       "MSHQ_jobcode dictionary_", 
                                       max(df$END_DATE), ".csv"),
            row.names = FALSE)
  print("The jobcode dictionary has been saved")
}

#### Format Data --------------------------------------------------------------
format_df <- df %>%
  left_join(mapping_jobcode, by = c("JOBCODE" = "JOBCODE")) %>%
  filter(PROVIDER == 0) %>%
  mutate(JOBCODE = substr(JOBCODE, 1, 10),
         POSITION_CODE_DESCRIPTION = substr(POSITION_CODE_DESCRIPTION, 1, 50),
         EMPLOYEE_NAME = substr(EMPLOYEE_NAME, 1, 30),
         WORKED_DEPARTMENT_NAME = substr(WORKED_DEPARTMENT_NAME, 1, 50),
         HOME_DEPARTMENT_NAME = substr(HOME_DEPARTMENT_NAME, 1, 50),
         HOME_DEPARTMENT = paste0(substr(HD_COA, 1, 3),
                                  substr(HD_COA, 41, 44),
                                  substr(HD_COA, 5, 7),
                                  substr(HD_COA, 12, 16))) %>%
  mutate(WD_EXPENSE = as.numeric(WD_EXPENSE),
         WD_HOURS = as.numeric(WD_HOURS)) %>%
  mutate(WORKED_DEPARTMENT = case_when(
    (WD_DEPARTMENT %in% cc_fundnum_conv & 
      WD_FUND_NUMBER != "00000000000") ~ WD_FUND_NUMBER,
    TRUE ~ WORKED_DEPARTMENT)) %>%
  mutate(WORKED_DEPARTMENT = case_when(
    (WORKED_DEPARTMENT %in% wd_fundnum_conv & 
      WD_FUND_NUMBER != "00000000000") ~ WD_FUND_NUMBER,
    TRUE ~ WORKED_DEPARTMENT)) %>%
  mutate(HOME_DEPARTMENT = case_when(
    (HD_DEPARTMENT %in% cc_fundnum_conv & 
       substr(HD_COA, 25, 35) != "00000000000") ~ substr(HD_COA, 25, 35),
    TRUE ~ HOME_DEPARTMENT)) %>%
  mutate(HOME_DEPARTMENT = case_when(
    (HOME_DEPARTMENT %in% wd_fundnum_conv & 
       substr(HD_COA, 25, 35) != "00000000000") ~ substr(HD_COA, 25, 35),
    TRUE ~ HOME_DEPARTMENT))
  
#### Jobcode Mapping ----------------------------------------------------------
# check if there are new combos of jc and worked department this month
if (nrow(format_df %>%
         left_join(jobcode_mapping_export, by = c("JOBCODE" = "Entity.Job.Code",
                                                  "WORKED_DEPARTMENT" = "Cost.Center.Code")) %>%
         filter(is.na(Premier.Standard.Job.Code))) > 0) {
  # if yes then create a jobcode mapping upload and save it
  jobcode_mapping <- format_df %>%
    left_join(jobcode_mapping_export, by = c("JOBCODE" = "Entity.Job.Code",
                                             "WORKED_DEPARTMENT" = "Cost.Center.Code")) %>%
    filter(is.na(Premier.Standard.Job.Code)) %>%
    select(PARTNER, WORKED_FACILITY, JOBCODE, WORKED_DEPARTMENT, JOBCODE_PREMIER) %>%
    distinct() %>%
    mutate(`Effective Start Date` = "1/1/2017",
           `Expiration Date` = "",
           `Premier Standard Dept Code` = "",
           `Allocation Percentage` = "100") %>%
    rename(`Corporation Code` = PARTNER,
           `Entity Code` = WORKED_FACILITY,
           `Entity Job Code` = JOBCODE,
           `Cost Center Code` = WORKED_DEPARTMENT,
           `Premier Standard Job Code` = JOBCODE_PREMIER) %>%
    select(`Effective Start Date`, `Expiration Date`, `Corporation Code`,
           `Entity Code`, `Entity Job Code`, `Cost Center Code`,
           `Premier Standard Dept Code`, `Premier Standard Job Code`,
           `Allocation Percentage`) %>%
    distinct()
  
  # save jobcode mapping
  write.csv(jobcode_mapping, paste0(data_dir, "Labor - Data/MSH/Payroll/",
                                    "MSH Labor/2.0/processed_files/",
                                    "jobcode_mapping/",
                                    "MSHQ_jobcode mapping_", 
                                    max(df$END_DATE), ".csv"),
            row.names = FALSE)
  print("The jobcode mappings have been saved")
}


## Cost Center Dictionary -----------------------------------------------------
home <- format_df %>%
  select(HOME_DEPARTMENT, HOME_DEPARTMENT_NAME) %>%
  rename(DEPARTMENT = HOME_DEPARTMENT,
         NAME = HOME_DEPARTMENT_NAME)
work <- format_df %>%
  select(WORKED_DEPARTMENT, WORKED_DEPARTMENT_NAME)%>%
  rename(DEPARTMENT = WORKED_DEPARTMENT,
         NAME = WORKED_DEPARTMENT_NAME)
# check if there are new cost centers
new_cost_centers <- rbind(home, work) %>%
  filter(!(DEPARTMENT %in% cost_center_export$Cost.Center.Code)) %>%
  distinct()
if (nrow(new_cost_centers) > 0) {
  # check if new cost centers have multiple descriptions
  if (length(unique(new_cost_centers$DEPARTMENT)) == nrow(new_cost_centers)) {
    print("All new cost centers have a unique description")
  } else {
    print("A new cost center has multiple descriptions")
  }
  # create cost center dictionary
  cost_center_dictionary <- new_cost_centers %>%
    mutate(`Corporation Code` = "729805",
           `Entity Code` = "NY0014") %>%
    rename(`Cost Center Code` = DEPARTMENT,
           `Cost Center Name` = NAME) %>%
    select(`Corporation Code`, `Entity Code`, 
           `Cost Center Code`, `Cost Center Name`)
  # save cost center dictionary
  write.csv(cost_center_dictionary, paste0(data_dir, "Labor - Data/MSH/Payroll",
                                           "/MSH Labor/2.0/processed_files/",
                                           "cost_center_dictionary/",
                                           "MSHQ_cost center dictionary_",
                                           max(df$END_DATE), ".csv"),
            row.names = FALSE)
  print("The cost center dictionary has been saved")
}

## Payroll Upload -------------------------------------------------------------
mapping_paycode <- tbl(oao_con, "LPM_MAPPING_PAYCODE") %>%
  collect() %>%
  select(PAYCODE_RAW, PAYCODE_PREMIER)

upload <- format_df %>%
  left_join(mapping_paycode, by = c("PAYCODE" = "PAYCODE_RAW")) %>%
  mutate(APPROVED_HOURS = "0") %>%
  group_by(PARTNER, HOME_FACILITY, HOME_DEPARTMENT, WORKED_FACILITY,
           WORKED_DEPARTMENT, START_DATE, END_DATE, EMPLOYEE_ID, 
           EMPLOYEE_NAME, APPROVED_HOURS, JOBCODE, PAYCODE_PREMIER) %>%
  summarise(Hours = round(sum(WD_HOURS), digits = 2),
            Expense = round(sum(WD_EXPENSE), digits = 2))
  

### Home Cost Center Correcton--------------------------------------------------
# check for employees wth multiple home cost centers during same time period
overlap_cc <- upload %>%
  group_by(EMPLOYEE_ID, START_DATE) %>%
  summarise(home_cost_centers = n_distinct(HOME_DEPARTMENT)) %>%
  right_join(upload,
             by = c("EMPLOYEE_ID" = "EMPLOYEE_ID",
                    "START_DATE" = "START_DATE")) %>%
  filter(home_cost_centers > 1) %>%
  mutate(unique_id = paste(EMPLOYEE_ID, HOME_DEPARTMENT, 
                           WORKED_DEPARTMENT, START_DATE, 
                           END_DATE, PAYCODE_PREMIER,
                           sep = "_"))

# if there are overlaps, save original for archival
if (nrow(overlap_cc) > 0) {
  write.csv(overlap_cc, paste0(data_dir, "Labor - Data/MSH/Payroll",
                                      "/MSH Labor/2.0/processed_files/",
                                      "home_cc_overlap/overlap_cc_",
                                      Sys.Date(), ".csv"),
            row.names = FALSE)
  
  # replace home cc with most common used home cc for each emp and start date combo
  cost_center_replacement <- overlap_cc %>%
    group_by(EMPLOYEE_ID, HOME_DEPARTMENT, START_DATE) %>%
    summarise(cost_center_count = n()) %>%
    group_by(EMPLOYEE_ID, START_DATE) %>%
    filter(cost_center_count == max(cost_center_count)) %>%
    rename(common_cc = HOME_DEPARTMENT) %>%
    distinct(EMPLOYEE_ID, START_DATE, cost_center_count,
             .keep_all = TRUE) %>%
    left_join(upload,
              by = c("EMPLOYEE_ID" = "EMPLOYEE_ID",
                     "START_DATE" = "START_DATE")) %>%
    mutate(HOME_DEPARTMENT = common_cc) %>%
    select(PARTNER, HOME_FACILITY, HOME_DEPARTMENT, WORKED_FACILITY,
           WORKED_DEPARTMENT, START_DATE, END_DATE, EMPLOYEE_ID, 
           EMPLOYEE_NAME, APPROVED_HOURS, JOBCODE, PAYCODE_PREMIER, 
           `Hours`, `Expense`)
  
  # remove employees original data from the upload df
  upload_no_overlap <- upload %>%
    mutate(unique_id = paste(EMPLOYEE_ID, HOME_DEPARTMENT, 
                             WORKED_DEPARTMENT, START_DATE, 
                             END_DATE, PAYCODE_PREMIER,
                             sep = "_")) %>%
    filter(!(unique_id %in% overlap_cc$unique_id)) %>%
    select(-unique_id) %>%
    rbind(cost_center_replacement) %>%
    group_by(PARTNER, HOME_FACILITY, HOME_DEPARTMENT, WORKED_FACILITY,
             WORKED_DEPARTMENT, START_DATE, END_DATE, EMPLOYEE_ID, 
             EMPLOYEE_NAME, APPROVED_HOURS, JOBCODE, PAYCODE_PREMIER) %>%
    summarise(Hours = round(sum(Hours), digits = 2),
              Expense = round(sum(Expense), digits = 2))
}

# restore upload object if it was edited for cost center overlaps
if (exists("upload_no_overlap")) {
  upload <- upload_no_overlap
  rm(upload_no_overlap)
}

### Date Overlap Correction ---------------------------------------------------
overlap_date <- upload %>%
  ungroup() %>%
  group_by(PARTNER, HOME_FACILITY, HOME_DEPARTMENT, WORKED_FACILITY,
           WORKED_DEPARTMENT, START_DATE, EMPLOYEE_ID, EMPLOYEE_NAME,
           APPROVED_HOURS, JOBCODE, PAYCODE_PREMIER) %>%
  mutate(dupe = n() > 1) %>%
  filter(dupe == TRUE) %>%
  select(-dupe) %>%
  mutate(unique_id = paste(EMPLOYEE_ID, HOME_DEPARTMENT, 
                           WORKED_DEPARTMENT, START_DATE, 
                           END_DATE, PAYCODE_PREMIER,
                           sep = "_"))

if (nrow(overlap_date) > 0) {
  write.csv(overlap_date, paste0(data_dir, "Labor - Data/MSH/Payroll",
                               "/MSH Labor/2.0/processed_files/",
                               "date_overlap/overlap_date_",
                               Sys.Date(), ".csv"),
            row.names = FALSE)
  
  # edit dates so data can be summarized to a single row
  date_replacement <- overlap_date %>%
    select(-unique_id) %>%
    group_by(PARTNER, HOME_FACILITY, HOME_DEPARTMENT, WORKED_FACILITY,
             WORKED_DEPARTMENT, EMPLOYEE_ID, EMPLOYEE_NAME,
             APPROVED_HOURS, JOBCODE, PAYCODE_PREMIER) %>%
    mutate(START_DATE = min(START_DATE),
           END_DATE = max(END_DATE)) %>%
    group_by(PARTNER, HOME_FACILITY, HOME_DEPARTMENT, WORKED_FACILITY,
             WORKED_DEPARTMENT, START_DATE, END_DATE, EMPLOYEE_ID, EMPLOYEE_NAME,
             APPROVED_HOURS, JOBCODE, PAYCODE_PREMIER) %>%
    summarise(Hours = round(sum(Hours), digits = 2),
              Expense = round(sum(Expense), digits = 2)) 
  
  # remove duplicate date rows from upload and combine
  upload_no_overlap <- upload %>%
    mutate(unique_id = paste(EMPLOYEE_ID, HOME_DEPARTMENT, 
                             WORKED_DEPARTMENT, START_DATE, 
                             END_DATE, PAYCODE_PREMIER,
                             sep = "_")) %>%
    filter(!(unique_id %in% overlap_date$unique_id)) %>%
    select(-unique_id) %>%
    rbind(date_replacement) 
}

# restore upload object if it was edited for cost center overlaps
if (exists("upload_no_overlap")) {
  upload <- upload_no_overlap
  rm(upload_no_overlap)
}

### Final Upload Creation -----------------------------------------------------
upload_final <- upload %>%
  mutate(START_DATE = paste0(substr(START_DATE, 6, 7), "/",
                             substr(START_DATE, 9, 10), "/",
                             substr(START_DATE, 1, 4)),
         END_DATE = paste0(substr(END_DATE, 6, 7), "/",
                           substr(END_DATE, 9, 10), "/",
                           substr(END_DATE, 1, 4))) %>%
  rename(`Corporation Code` = PARTNER,
         `Home Entity Code` = HOME_FACILITY,
         `Home Cost Center Code` = HOME_DEPARTMENT,
         `Worked Entity Code` = WORKED_FACILITY,
         `Worked Cost Center Code` = WORKED_DEPARTMENT,
         `Start Date` = START_DATE,
         `End Date` = END_DATE,
         `Employee Code` = EMPLOYEE_ID,
         `Employee Name` = EMPLOYEE_NAME,
         `Approved Hours per Pay Period` = APPROVED_HOURS,
         `Job Code` = JOBCODE,
         `Pay Code` = PAYCODE_PREMIER)


# save premier upload
write.csv(upload_final, paste0(data_dir, "Labor - Data/MSH/Payroll",
                                    "/MSH Labor/2.0/processed_files/",
                                    "uploads/","MSHQ_Payroll_",
                                    min(mdy(upload_final$`Start Date`)), "_", 
                                    max(mdy(upload_final$`End Date`)),".csv"),
          row.names = FALSE)
