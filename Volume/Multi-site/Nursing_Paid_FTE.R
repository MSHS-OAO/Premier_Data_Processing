# Libraries -------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(dbplyr)
library(lubridate)
library(DBI)
library(odbc)
library(glue)

# Directories -----------------------------------------------------------------
msbib_rightsourcing_dir <- paste0("/SharedDrive/deans/Presidents/SixSigma/",
                                  "MSHS Productivity/Productivity/",
                                  "Labor - Data/Rightsourcing Labor/MSBIB/",
                                  "uploads")
mshq_rightsourcing_dir <- paste0("/SharedDrive/deans/Presidents/SixSigma/",
                                  "MSHS Productivity/Productivity/",
                                  "Labor - Data/Rightsourcing Labor/MSHQ/",
                                  "uploads")
msmw_rightsourcing_dir <- paste0("/SharedDrive/deans/Presidents/SixSigma/",
                                 "MSHS Productivity/Productivity/",
                                 "Labor - Data/Rightsourcing Labor/MSMW/Uploads")
nursing_paid_fte_dir <- paste0("/SharedDrive/deans/Presidents/SixSigma/",
                               "MSHS Productivity/Productivity/Volume - Data/",
                               "Multisite Volumes/Nursing Paid FTE/")
# OAO_PRODUCTION DB connection
con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")

# Data References -------------------------------------------------------------
nursing_paid_fte_cc <- read.csv(paste0(nursing_paid_fte_dir, 
                                       "mapping/nursing_paid_fte_costcenters2.csv"),
                                colClasses = rep("character", 3))

paycycle_mapping <- tbl(con_prod, "LPM_MAPPING_PAYCYCLE") %>%
  select(PAYCYCLE_DATE, PP_START_DATE, PP_END_DATE) %>%
  collect()

# Data Import -----------------------------------------------------------------
# import most recent msbib rightsourcing upload
msbib_files <- file.info(list.files(msbib_rightsourcing_dir, full.names = T))
msbib_agency <- read.csv(rownames(msbib_files)[which.max(msbib_files$mtime)], 
                         header = T,sep = ",", stringsAsFactors = F, 
                         colClasses = rep("character", 14))
# get start and end date range from rightsourcing upload
msbib_end <- max(mdy(msbib_agency$End.Date))
msbib_start <- min(mdy(msbib_agency$Start.Date))
if (paycycle_mapping %>% 
    filter(PAYCYCLE_DATE == msbib_start) %>% select(PAYCYCLE_DATE) %>% pull() ==
    paycycle_mapping %>% 
    filter(PAYCYCLE_DATE == msbib_start) %>% select(PP_START_DATE) %>% pull()) {
  msbib_start <- msbib_start
} else {
  start_dates <- paycycle_mapping %>%
    filter(PAYCYCLE_DATE >= msbib_start) %>%
    select(PP_START_DATE) %>%
    distinct() %>% 
    arrange(PP_START_DATE)
  msbib_start <- pull(start_dates[2,])
}

# import most recent mshq rightsourcing upload
mshq_files <- file.info(list.files(mshq_rightsourcing_dir, full.names = T))
mshq_agency <- read.csv(rownames(mshq_files)[which.max(mshq_files$mtime)], 
                        header = T,sep = ",", stringsAsFactors = F, 
                        colClasses = rep("character", 14))
# get start and end date range from rightsourcing upload
mshq_end <- max(mdy(mshq_agency$End.Date))
mshq_start <- min(mdy(mshq_agency$Start.Date))
if (paycycle_mapping %>% 
    filter(PAYCYCLE_DATE == mshq_start) %>% select(PAYCYCLE_DATE) %>% pull() ==
    paycycle_mapping %>% 
    filter(PAYCYCLE_DATE == mshq_start) %>% select(PP_START_DATE) %>% pull()) {
      mshq_start <- mshq_start
    } else {
      start_dates <- paycycle_mapping %>%
        filter(PAYCYCLE_DATE >= mshq_start) %>%
        select(PP_START_DATE) %>%
        distinct() %>% 
        arrange(PP_START_DATE)
      mshq_start <- pull(start_dates[2,])
    }

# import most recent mshq rightsourcing upload
msmw_files <- file.info(list.files(msmw_rightsourcing_dir, full.names = T))
msmw_agency <- read.csv(rownames(msmw_files)[which.max(msmw_files$mtime)], 
                        header = T,sep = ",", stringsAsFactors = F, 
                        colClasses = rep("character", 14))
# get start and end date range from rightsourcing upload
msmw_end <- max(mdy(msmw_agency$End.Date))
msmw_start <- min(mdy(msmw_agency$Start.Date))
if (paycycle_mapping %>% 
    filter(PAYCYCLE_DATE == msmw_start) %>% select(PAYCYCLE_DATE) %>% pull() ==
    paycycle_mapping %>% 
    filter(PAYCYCLE_DATE == msmw_start) %>% select(PP_START_DATE) %>% pull()) {
  msmw_start <- msmw_start
} else {
  start_dates <- paycycle_mapping %>%
    filter(PAYCYCLE_DATE >= msmw_start) %>%
    select(PP_START_DATE) %>%
    distinct() %>% 
    arrange(PP_START_DATE)
  msmw_start <- pull(start_dates[2,])
}

# get list of nursing paid fte cost centers
nursing_cost_centers <- paste(nursing_paid_fte_cc$COST_CENTER, 
                              collapse = "\',\'")

# msbib DB query
bislr_db_query <- glue(
  "SELECT * FROM LPM_MAPPED_BISLR_ORACLE
   WHERE PP_END_DATE >= DATE {glue::single_quote(msbib_start)} AND
         PP_END_DATE <= DATE {glue::single_quote(msbib_end)} AND
         PROVIDER = 0 AND
         INCLUDE_HOURS = 1 AND
         WORKED_DEPARTMENT IN (\'{nursing_cost_centers}\');"
)

# mshq DB query
mshq_db_query <- glue(
  "SELECT * FROM LPM_MAPPED_MSHQ_ORACLE
   WHERE PP_END_DATE >= DATE {glue::single_quote(mshq_start)} AND
         PP_END_DATE <= DATE {glue::single_quote(mshq_end)} AND
         PROVIDER = 0 AND
         INCLUDE_HOURS = 1 AND
         WORKED_DEPARTMENT IN (\'{nursing_cost_centers}\');"
)

# create, execute and clear result for bislr payroll query
bislr_db_query <- dbSendQuery(con_prod, bislr_db_query)
bislr_payroll <- dbFetch(bislr_db_query)
dbClearResult(bislr_db_query)
# create, execute and clear result for mshq payroll query
mshq_db_query <- dbSendQuery(con_prod, mshq_db_query)
mshq_payroll <- dbFetch(mshq_db_query)
dbClearResult(mshq_db_query)

# Data Pre-processing ---------------------------------------------------------
# combine rightsourcing tables
rightsourcing_agency <- rbind(msbib_agency, mshq_agency, msmw_agency)
# combine DB tables
mshs_payroll <- rbind(bislr_payroll, mshq_payroll)

## PP End Date & Site ---------------------------------------------------------
# rightsourcing agency
rightsourcing_agency <- rightsourcing_agency %>%
  mutate(End.Date = mdy(End.Date)) %>%
  left_join(paycycle_mapping, 
            by = c("End.Date" = "PAYCYCLE_DATE")) %>%
  left_join(nursing_paid_fte_cc, 
            by = c("Worked.Cost.Center.Code" = "COST_CENTER"))

# mshs payroll
mshs_payroll <- mshs_payroll %>%
  select(-PP_END_DATE) %>%
  left_join(paycycle_mapping,
            by = c("END_DATE" = "PAYCYCLE_DATE")) %>%
  left_join(nursing_paid_fte_cc, 
            by = c("WORKED_DEPARTMENT" = "COST_CENTER"))

## Data Aggregation -----------------------------------------------------------
# rightsourcing aggregation
rightsourcing_agency <- rightsourcing_agency %>%
  filter(!is.na(SITE)) %>%
  group_by(SITE, PP_START_DATE, PP_END_DATE) %>%
  summarise(PAID_FTE = sum(as.numeric(Hours))/75)
# payroll aggregation
mshs_payroll <- mshs_payroll %>%
  group_by(SITE, PP_START_DATE, PP_END_DATE) %>%
  summarise(PAID_FTE = sum(as.numeric(WD_HOURS))/75)

# combine agency and payroll
mshs_nursing_paid_FTE <- rbind(rightsourcing_agency, mshs_payroll) %>%
  group_by(SITE, PP_START_DATE, PP_END_DATE) %>%
  summarise(PAID_FTE = round(sum(PAID_FTE), digits = 2))

# Quality Checks ----------------------------------------------------------
# check to make sure there are 4 pay periods for each site for each labor source
# visual check for paid FTE values
View(mshs_nursing_paid_FTE)

# check number of pay periods per site for agency paid FTE
pp_check_agency <- rbind(rightsourcing_agency) %>%
  group_by(SITE) %>%
  summarise(PP_COUNT = n())
View(pp_check_agency)

# check number of pay periods per site for payroll paid FTE
pp_check_payroll <- mshs_payroll %>%
  group_by(SITE) %>%
  summarise(PP_COUNT = n())
View(pp_check_payroll)

# Data Formatting ---------------------------------------------------------
# put data in premier upload format
upload <- mshs_nursing_paid_FTE %>%
  mutate('Corporation Code' = '729805',
         'Entity Code' = case_when(
           SITE %in% c('MSBI', 'MSB') ~ '630571',
           SITE %in% c('MSH', 'MSQ') ~ 'NY0014',
           SITE == 'MSM' ~ 'NY2163',
           SITE == 'MSW' ~ 'NY2162'),
         'Start Date' = paste0(substr(PP_START_DATE, 6, 7), "/",
                               substr(PP_START_DATE, 9, 10), "/",
                               substr(PP_START_DATE, 1, 4)),
         'End Date' = paste0(substr(PP_END_DATE, 6, 7), "/",
                             substr(PP_END_DATE, 9, 10), "/",
                             substr(PP_END_DATE, 1, 4)),
         'Cost Center Code' = case_when(
           SITE == 'MSBI'~ '401000040410101',
           SITE == 'MSB' ~ '402000040710101',
           SITE == 'MSH' ~ '101000010110101',
           SITE == 'MSM' ~ '302000030210101',
           SITE == 'MSW' ~ '301000030110101',
           SITE == 'MSQ' ~ '102000010710101'),
         'Volume Code' = case_when(
           SITE == 'MSBI'~ '401404101011',
           SITE == 'MSB' ~ '402407101011',
           SITE == 'MSH' ~ '101101101011',
           SITE == 'MSM' ~ '302000101011',
           SITE == 'MSW' ~ '301000101011',
           SITE == 'MSQ' ~ '102107101011'),
         'Budget Volume' = '0') %>%
  rename('Actual Volume' = PAID_FTE) %>%
  ungroup() %>%
  select(`Corporation Code`, `Entity Code`, `Cost Center Code`, `Start Date`,
         `End Date`, `Volume Code`, `Actual Volume`, `Budget Volume`)
# File Saving -------------------------------------------------------------
write.csv(upload, paste0(nursing_paid_fte_dir, "uploads/MSHS_Nursing Paid FTE_",
                         min(c(msbib_start, mshq_start)), "_",
                         max(c(msbib_end, mshq_end)),
                         ".csv"),
          row.names = FALSE)
# Script End --------------------------------------------------------------