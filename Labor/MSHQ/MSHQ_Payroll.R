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

## Read Labor -----------------------------------------------------------------
df <- file.info(list.files(paste0(data_dir, "Universal Data/Labor/Raw Data/",
                                  "MSHQ_sftp_sync/Insert/")
                           , full.names = T))
df <- read.csv(rownames(df)[which.max(df$mtime)], header = T, sep = "~",
               stringsAsFactors = F, colClasses = rep("character", 33)) %>%
  filter(!is.na(JOBCODE))

## Read Mapping Files ---------------------------------------------------------
oao_con <- dbConnect(odbc(), "OAO Cloud DB Production")
greg_con <- dbConnect(odbc(), "OAO Cloud DB Greg")

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
         WD_HOURS = as.numeric(WD_HOURS))

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

## Payroll Upload -----------------------------------------------------
mapping_paycode <- tbl(oao_con, "LPM_MAPPING_PAYCODE") %>%
  collect() %>%
  select(PAYCODE_RAW, PAYCODE_PREMIER)

upload <- format_df %>%
  left_join(mapping_paycode, by = c("PAYCODE" = "PAYCODE_RAW")) %>%
  mutate(APPROVED_HOURS = "0",
         START_DATE = paste0(substr(START_DATE, 6, 7), "/",
                             substr(START_DATE, 9, 10), "/",
                             substr(START_DATE, 1, 4)),
         END_DATE = paste0(substr(END_DATE, 6, 7), "/",
                           substr(END_DATE, 9, 10), "/",
                           substr(END_DATE, 1, 4))) %>%
  group_by(PARTNER, HOME_FACILITY, HOME_DEPARTMENT, WORKED_FACILITY,
           WORKED_DEPARTMENT, START_DATE, END_DATE, EMPLOYEE_ID, 
           EMPLOYEE_NAME, APPROVED_HOURS, JOBCODE, PAYCODE_PREMIER) %>%
  summarise(Hours = round(sum(WD_HOURS), digits = 2),
            Expense = round(sum(WD_EXPENSE), digits = 2)) %>%
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
write.csv(upload, paste0(data_dir, "Labor - Data/MSH/Payroll",
                         "/MSH Labor/2.0/processed_files/",
                         "uploads/","MSHQ_Payroll_",
                         min(mdy(upload$`Start Date`)), "_", 
                         max(mdy(upload$`End Date`)),".csv"),
          row.names = FALSE)
