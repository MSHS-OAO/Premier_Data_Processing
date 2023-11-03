library(DBI)
library(odbc)
library(dplyr)
library(dbplyr)
library(glue)

library(readxl)
library(xlsx)
library(lubridate)
library(ggplot2)
library(writexl)
library(rstudioapi)

# set up connection to schema
# setting up in Workbench should not require password entry
 con <- dbConnect(odbc(), "OracleODBC-21_5",
                 uid = "TBD",
                 pwd = "TBD")



j_drive <- paste0("/SharedDrive/deans/Presidents")


## Pay cycles -------------------------------------------------------------
dict_pay_cycles <- read_xlsx(
  paste0(
    j_drive, "/SixSigma/MSHS Productivity/Productivity/Universal Data/",
    "Mapping/MSHS_Pay_Cycle.xlsx"
  ),
  col_types = c("date", "date", "date", "skip")
  # bringing in the date as "text" results in the excel number representation
  # of a date
)

# dates originally come in as POSIXct, so they're being converted to Date
dict_pay_cycles <- dict_pay_cycles %>%
  mutate(#DATE = format(as.Date(DATE), "%m/%d/%Y"),
         START.DATE = format(as.Date(START.DATE), "%m/%d/%Y"),
         END.DATE = format(as.Date(END.DATE), "%m/%d/%Y"))


###

path_dict_prem <- paste0(j_drive,
                         "/SixSigma/MSHS Productivity/Productivity",
                         "/Volume - Data/MSBI Data/Union Square/",
                         "R Code/MSUS Epic Dictionary.xlsx")

dict_epic <- read_xlsx(
  path_dict_prem,
  sheet = 1,
  skip = 0
)

dict_epic_short <- dict_epic %>%
  select(`Epic Department Name`, `Volume ID`, `Cost Center`, `volume ratio`)

epic_dpts <- unique(dict_epic_short$`Epic Department Name`)

### rehab offsite ---------------------------------------------------------

dict_rehab_docs <- read_xlsx(
  path_dict_prem,
  sheet = 2,
  skip = 0
)

# we can now recreate the queries we ran in DataGrip
# note this query does not retrieve data as a df. Conceptually it is similar to a view
df <- tbl(con, 
          in_schema("VILLEA04", "AMBULATORY_ACCESS_VIEW")) %>%
  filter(DEPARTMENT %in% epic_dpts,
         APPT_DATE_YEAR >= as.Date("2023-08-27"),
         APPT_DATE_YEAR <= as.Date("2023-09-23")) %>%
  select(DEPARTMENT, DEPARTMENT_ID, APPT_DATE_YEAR, APPT_STATUS, PROVIDER) %>%
  show_query() %>%
  collect()

data_epic <- df %>%
  left_join(dict_epic_short, by = c("DEPARTMENT" = "Epic Department Name")) %>%
  left_join(dict_pay_cycles,
            by = c("APPT_DATE_YEAR" = "DATE")) %>%
  filter(APPT_STATUS %in% c("Completed", "Arrived", "Checked in", "Checked out"))


## Visit counter ----------------------------------------------------------

# rehab offsite docs that are not included docs get 0
# all others keep their ratio
data_epic <- data_epic %>%
  rename(volume = `volume ratio`) %>%
  mutate(volume = case_when(
    DEPARTMENT %in% dict_rehab_docs$`Epic Department Name` &
      !(PROVIDER %in% dict_rehab_docs$Provider) ~ 0,
    TRUE ~ volume
  ))


## upload summary ---------------------------------------------------------
summary_upload <- data_epic %>%
  group_by(`Cost Center`, START.DATE, END.DATE, `Volume ID`) %>%
  summarize(visits = sum(volume)) %>%
  mutate(visits = round(visits, digits = 0)) %>%
  ungroup() %>%
  filter(!is.na(`Volume ID`)) %>%
  filter(!(`Volume ID` %in% c("TBD", "X")))


## Add rows for volume ID with 0 volume ------------------------------------

data_dates <- data_epic %>%
  select(START.DATE, END.DATE) %>%
  unique()

dict_epic_unique <- dict_epic %>%
  select(`Cost Center`, `Volume ID`) %>%
  unique()

dict_and_date <- merge(data_dates, dict_epic_unique)

missing_vol_id_date <- dict_and_date %>%
  anti_join(data_epic)

zero_rows <- missing_vol_id_date %>%
  mutate(visits = 0) %>%
  relocate(`Cost Center`, .before = START.DATE)

zero_depts <- dict_epic_short %>%
  filter(`Volume ID` %in% zero_rows$`Volume ID`)

# would be better to display Cost Center name and the volume IDs
if (length(unique(zero_depts$`Epic Department Name`)) > 0) {
  winDialog(message = paste0(
    "These Departments had a pay period with 0 volume:\r\r",
    paste(sort(unique(zero_depts$`Epic Department Name`)),
          collapse = "\n\n")),
    type = "ok")
}


### Combining with upload summary --------------------------------------------
summary_upload <- rbind(summary_upload, zero_rows)


# Data Formatting ---------------------------------------------------------

upload_file <- summary_upload %>%
  mutate(entity = "729805",
         facility = "630571",
         budget = "0") %>%
  relocate(c(entity, facility), .before = `Cost Center`)

# File Saving -------------------------------------------------------------

# date_min_char <- format(as.Date(data_date_min, "%m/%d/%Y"), "%Y-%m-%d")
# date_max_char <- format(as.Date(data_date_max, "%m/%d/%Y"), "%Y-%m-%d")

file_name_premier <-
  paste0("MSDUS_Department Volumes_", "MM_db_test", #date_min_char, "_to_", date_max_char,
         ".csv")
path_folder_premier_export <- paste0(j_drive,
                                     "/SixSigma/MSHS Productivity/Productivity",
                                     "/Volume - Data/MSBI Data/Union Square",
                                     "/Calculation Worksheets")

upload_cols <- c("Corporation Code",
                 "Entity Code",
                 "Cost Center Code",
                 "Start Date",
                 "End Date",
                 "Volume Code",
                 "Actual Volume",
                 "Budget Volume")

colnames(upload_file) <- upload_cols

write.table(
  upload_file,
  file = paste0(path_folder_premier_export, "/", file_name_premier),
  row.names = F,
  col.names = T,
  sep = ","
)

message(paste0("\nUpload file written to:\n",
               paste0(path_folder_premier_export, "/", file_name_premier,
                      "\n")))

write.table(
  df,
  file = paste0(path_folder_premier_export, "/", "raw_pull.csv"),
  row.names = F,
  col.names = T,
  sep = ","
)

# Script End --------------------------------------------------------------
