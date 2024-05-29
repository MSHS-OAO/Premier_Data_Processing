library(odbc)
library(DBI)
library(dplyr)
library(dbplyr)
library(tidyr)

# start and end dates
start <- "2024-03-24"
end <- "2024-04-20"

oao_prod_dsn <- "OAO Cloud DB Production"
oao_prod_conn <- dbConnect(odbc(),
                           oao_prod_dsn)

# read in epic ID mapping
epic_mapping <- read.csv(paste0("/SharedDrive/deans/Presidents/SixSigma/",
                                "MSHS Productivity/Productivity/Analysis/",
                                "MSH Ambulatory/Volume Data/Volume Validation/",
                                "EpicID_Mapping.csv"), 
                         colClasses = rep("character", 5))
epic_id <- pull(epic_mapping, EPIC_ID)

# read in paycycle mapping table
paycycle_mapping <- tbl(oao_prod_conn, "LPM_MAPPING_PAYCYCLE") %>%
  collect()

# read in source data
source_df <- tbl(oao_prod_conn, "MV_DM_PATIENT_ACCESS") %>%
  select(DEPARTMENT_ID, APPT_DTTM, DERIVED_STATUS_DESC) %>%
  filter(APPT_DTTM >= as.Date(start),
         APPT_DTTM <= as.Date(end),
         DEPARTMENT_ID %in% epic_id,
         DERIVED_STATUS_DESC == "Arrived") %>%
  show_query() %>%
  collect() %>%
  mutate(DEPARTMENT_ID = as.character(DEPARTMENT_ID)) %>%
  mutate(DATE = as.Date(APPT_DTTM)) %>%
  left_join(epic_mapping, by = c("DEPARTMENT_ID" = "EPIC_ID")) %>%
  left_join(paycycle_mapping, by = c("DATE" = "PAYCYCLE_DATE"))

 export <- source_df %>%
  group_by(PP_START_DATE, PP_END_DATE, COST_CENTER, VOLUME_CODE) %>%
  summarise(VISITS = n()) %>%
  mutate(`Corporation Code` = "729805",
         `Entity Code` = "NY0014",
         `Cost Center Code` = COST_CENTER,
         `Start Date` = paste0(substr(PP_START_DATE, 6, 7), "/",
                               substr(PP_START_DATE, 9, 10), "/",
                               substr(PP_START_DATE, 1, 4)),
         `End Date` = paste0(substr(PP_END_DATE, 6, 7), "/",
                             substr(PP_END_DATE, 9, 10), "/",
                             substr(PP_END_DATE, 1, 4)),
         `Volume Code` = VOLUME_CODE,
         `Actual Volume` = VISITS,
         `Budget Volume` = "0") %>%
  ungroup() %>%
  select(`Corporation Code`, `Entity Code`, `Cost Center Code`, `Start Date`,
         `End Date`, `Volume Code`, `Actual Volume`, `Budget Volume`)

write.table(export, paste0("/SharedDrive/deans/Presidents/SixSigma/",
                           "MSHS Productivity/Productivity/Analysis/",
                           "MSH Ambulatory/Volume Data/MSH_AMBULATORY VOLUME_",
                           start, "_", end, ".csv"),
            row.names = F, sep = ",")



