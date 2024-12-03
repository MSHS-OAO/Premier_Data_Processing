
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(xlsx)
library(rstudioapi)
# library(stringr) This is in tidyverse
library(stringi)
library(DBI)
library(odbc)

# conflicted::conflicts_prefer(dplyr::filter)

# clear memory of all objects including those hidden
rm(list = ls(all.names = TRUE))

# Directories -------------------------------------------------------------
dir <- "/SharedDrive/deans/Presidents/SixSigma/MSHS Productivity/Productivity"
dir_BISLR <- paste0(dir, "/Labor - Data/Multi-site/BISLR")
dir_universal <- paste0(dir, "/Universal Data")

oao_con <- dbConnect(odbc(), "OAO Cloud DB Production")
# oao_con_dev <- dbConnect(odbc(), "OAO Cloud DB Development")

# User Warnings -----------------------------------------------------------

showDialog(
  title = "Updated Premier Dictionary Mapping Files",
  message = paste0(
    "USER WARNING: ",
    "Before proceeding, ",
    "be sure that all the latest Premier Dictionary and Mapping Exports ",
    "have been saved in the folder listed below.  ",
    "    Issues can arise if the files are not the latest version.     ",
    dir, "/Universal Data/Premier")
  )

# Constants ---------------------------------------------------------------
new_dpt_map <- 10095
map_effective_date_dpt <- as.Date("2016-01-01")
corp_code <- 729805
jc_alloc_pct <- 100

accrual_report_ids <- c("DNU_8600", "DNU_MSM_8600", "DNU_MSW_8600")
true_accrual_cc_desc <- c("ACCRUAL COST CENTER", "SPECIAL FUNDS ACCOUNTING",
                          "GENERAL ACCOUNTING", "FPA BIMG")

dummy_report_ids <- c("DNU_000", "DNU_MSM000", "DNU_MSW000")

jc_desc_threshold <- 5

# list of dept IDs where fund number should be used in place of the
# standard department ID structure
cc_fundnum_conv <- c(37800, 37803)

## Premier Formatting ------------------------------------------------------
char_len_dpt <- 15
char_len_dpt_name <- 50
char_len_employ <- 30
char_len_jobcode <- 10
char_len_paycode <- 15
char_len_paycode_name <- 50

# Functions --------------------------------------------------------------
import_recent_file <- function(folder.path, place) {
  # Importing File information from Folder
  File.Name <- list.files(path = folder.path, full.names = F)
  File.Path <- list.files(path = folder.path, full.names = T)
  File.Date <- as.Date(sapply(File.Name,
                              function(x) {
                                if (substr(x, nchar(x) - 3,
                                           nchar(x)) == ".txt") {
                                  substr(x, nchar(x) - 18, nchar(x) - 9)
                                } else {
                                  (substr(x, nchar(x) - 14, nchar(x) - 5))
                                }
                              }),
                       format = "%m_%d_%Y")
  File.Table <<- data.table::data.table(File.Name, File.Date, File.Path) %>%
    arrange(desc(File.Date))
  # Quality Check - Confirming Upload File
  answer <- showQuestion(title = "Question",
                         message = paste0("File selected is: \n",
                                          File.Table$File.Name[place], "\n",
                                          "Is this the correct file?"),
                         ok = "Yes", cancel = "No")
  if (answer == F) {
    user_selected_file <- select.list(choices = File.Table$File.Name,
                                      multiple = F,
                                      title = "Select the correct file",
                                      graphics = T)
    place <- grep(user_selected_file, File.Table$File.Name)
    cat("File selected is ", File.Table$File.Name[place])
  }
  # Importing Data
  data_recent <- read.csv(File.Table$File.Path[place],
                          header = T, sep = "~", fill = T)
  return(data_recent)
}

# Import Data -------------------------------------------------------------
raw_payroll <- import_recent_file(paste0(dir_BISLR, "/Source Data"), 1)

# Import References -------------------------------------------------------
pay_cycles_uploaded <- read_xlsx(paste0(dir_BISLR, "/Reference",
                                        "/Pay cycles uploaded_Tracker.xlsx"),
                                 sheet = 1)
pay_cycles_uploaded_raw <- pay_cycles_uploaded
msus_removal_list <- read_xlsx(paste0(dir_BISLR,
                                      "/Reference/MSUS_removal_list.xlsx"),
                               sheet = 1)
## Universal Reference Files -----------------------------------------------
map_uni_paycodes <- tbl(oao_con, "LPM_MAPPING_PAYCODE") %>%
  collect()
map_uni_jobcodes <- tbl(oao_con, "LPM_MAPPING_JOBCODE") %>%
  collect()
map_uni_reports <- tbl(oao_con, "LPM_MAPPING_REPDEF") %>%
  collect()
map_uni_cost_ctr <- tbl(oao_con, "LPM_MAPPING_COST_CENTER") %>%
  collect()
map_uni_key_vol <- tbl(oao_con, "LPM_MAPPING_KEY_VOLUME") %>%
  collect()
map_uni_paycycles <- tbl(oao_con, "LPM_MAPPING_PAYCYCLE") %>%
  collect()


## Premier Reference Files -------------------------------------------------
dict_premier_dpt <- read.csv(paste0(dir_universal,
                                    "/Premier/2.0 Dictionary Exports",
                                    "/MSTCC.csv"),
                             col.names = c("Corporation.Code", "Site",
                                           "Cost.Center",
                                           "Cost.Center.Description"),
                             sep = ",")
dict_premier_jobcode <- read.csv(paste0(dir_universal,
                                        "/Premier/2.0 Dictionary Exports",
                                        "/MSTJOBCODE.csv"),
                                 col.names = c("Corporation.Code", "Site",
                                               "Job.Code",
                                               "Job.Code.Description",
                                               "Default.Agency.Hourly.Rate",
                                               "Effective Start Date",
                                               "Expiration Date"),
                                 sep = ",")
dict_premier_report <- read.csv(paste0(dir_universal,
                                       "/Premier/2.0 Dictionary Exports",
                                       "/MDDCCREL.csv"),
                                col.names = c("Corporation.Code", "Site",
                                              "Report.ID", "Cost.Center", 
                                              "Effective.Date", 
                                              "Expiration Date",
                                              "Delete Cost Center Association"),
                                sep = ",")

dict_premier_paycode <- read.csv(paste0(dir_universal,
                                        "/Premier/2.0 Dictionary Exports",
                                        "/MSTPAYCODE.csv"),
                                 col.names = c("Partner.or.Health.System.ID",
                                               "Facility.or.Hospital.ID",
                                               "Pay.Code", "Pay.Code.Name"),
                                 sep = ",")
map_premier_jobcode <- read.csv(paste0(dir_universal,
                                       "/Premier/2.0 Mapping Exports",
                                       "/MAPJOBCODE.csv"),
                                col.names = c("Effective.Start.Date",
                                              "Expiration.Date",
                                              "Corporation.Code", "Site",
                                              "Job.Code",
                                              "Cost.Center.Code",
                                              "Premier.Standard.Dept.Code",
                                              "Premier.Standard.Job.Code",
                                              "Allocation.Percentage",
                                              "Job.Code.Name"),
                                sep = ",")

## Quality - piv_wide_check ------------------------------------------------

piv_wide_check_prev <- read.csv(paste0(dir_BISLR, "/Quality Checks",
                                       "/piv_wide_check", ".csv"))

# Preprocessing --------------------------------------------------------------


## References --------------------------------------------------------------
map_uni_jobcodes <- map_uni_jobcodes %>%
  rename(J.C = JOBCODE,
         PAYROLL = PAYROLL,
         J.C.DESCRIPTION = JOBCODE_DESCRIPTION,
         PROVIDER = PROVIDER,
         PREMIER.J.C = JOBCODE_PREMIER,
         PREMIER.J.C.DESCRIPTION = JOBCODE_PREMIER_DESCRIPTION) %>%
  mutate(J.C = str_trim(J.C)) %>%
  mutate(JC_in_UniversalFile = 1)
map_uni_paycodes <- map_uni_paycodes %>%
  rename(RAW.PAY.CODE = PAYCODE_RAW,
         PAY.CODE = PAYCODE_PREMIER,
         PAY.CODE.NAME = PAYCODE_DESCRIPTION,
         PAY.CODE.CATEGORY = PAYCODE_CATEGORY,
         INCLUDE.HOURS = INCLUDE_HOURS,
         INCLUDE.EXPENSES = INCLUDE_EXPENSES,
         WORKED.PAY.CODE = WORKED_PAYCODE) %>%
  mutate(Paycode_in_Universal = 1)
pay_cycles_uploaded <- pay_cycles_uploaded %>%
  select(-capture_time) %>%
  mutate(Pay_Cycle_Uploaded = 1)
dict_premier_dpt <- dict_premier_dpt %>%
  mutate(Cost.Center = as.character(Cost.Center), Dpt_in_Dict = 1)
dict_premier_jobcode <- dict_premier_jobcode %>%
  select(1:4) %>%
  mutate(JC_in_Dicty = 1)
map_premier_jobcode <- map_premier_jobcode %>%
  filter(Premier.Standard.Job.Code != "9999") %>%
  select(Corporation.Code, Site, Job.Code, Cost.Center.Code) %>%
  mutate(JC_in_Dict = 1)

report_list <- dict_premier_report %>%
  filter(Report.ID != "--UCCD--") %>%
  select(Cost.Center, Site, Report.ID) %>%
  distinct()
# In the future, there's potential for a cost center to show up in multiple
# reports if we begin moving cost centers across reports.


# modifying column names in order to not have to recode the rest of the script
map_uni_paycycles <- map_uni_paycycles %>%
  rename(DATE = PAYCYCLE_DATE,
         START.DATE = PP_START_DATE,
         END.DATE = PP_END_DATE,
         PREMIER.DISTRIBUTION = PREMIER_DISTRIBUTION)

map_uni_reports_orig <- map_uni_reports
map_uni_reports <- map_uni_reports %>%
  left_join(map_uni_cost_ctr, relationship = "many-to-many") %>%
  left_join(map_uni_key_vol, relationship = "many-to-many") %>%
  rename(DEFINITION.CODE = DEFINITION_CODE,
         DEFINITION.NAME = DEFINITION_NAME,
         KEY.VOLUME = KEY_VOLUME,
         COST.CENTER = LEGACY_COST_CENTER,
         ORACLE.COST.CENTER = ORACLE_COST_CENTER,
         COST.CENTER.DESCRIPTION = COST_CENTER_DESCRIPTION,
         CORPORATE.SERVICE.LINE = CORPORATE_SERVICE_LINE,
         SITE = SITE,
         CLOSED = CLOSED,
         VP = VP,
         DEPARTMENT.BREAKDOWN = DEPARTMENT_BREAKDOWN)
  

dist_dates <- map_uni_paycycles %>%
  select(END.DATE, PREMIER.DISTRIBUTION) %>%
  distinct() %>%
  drop_na() %>%
  arrange(END.DATE) %>%
  filter(PREMIER.DISTRIBUTION %in% c(TRUE, 1),
         END.DATE < max(as.POSIXct(raw_payroll$End.Date, format = "%m/%d/%Y")))

# Selecting the most recent distribution date
distribution_date <- max(as.POSIXct(dist_dates$END.DATE))

dist_prev <- dist_dates$END.DATE[
  which(dist_dates$END.DATE == distribution_date) - 1
]

## Site Hours Quality Check ------------------------------------------------
piv_wide_check <- raw_payroll %>%
  filter(as.Date(End.Date, "%m/%d/%Y") >= dist_prev &
           as.Date(End.Date, "%m/%d/%Y") <= distribution_date +
                                            lubridate::days(7)) %>%
  group_by(Facility.Hospital.Id_Worked, Payroll.Name, End.Date) %>%
  summarize(Hours = sum(as.numeric(Hours), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(as.Date(End.Date, "%m/%d/%Y"),
          Facility.Hospital.Id_Worked, Payroll.Name) %>%
  bind_rows(summarize(group_by(., Facility.Hospital.Id_Worked, End.Date),
                      Hours = sum(Hours, na.rm = TRUE),
                      Payroll.Name = "-SITE TOTAL-")) %>%
  bind_rows(summarize(group_by(filter(., Payroll.Name == "-SITE TOTAL-"),
                               End.Date),
                      Hours = sum(Hours, na.rm = TRUE),
                      across(where(is.character), ~"TOTAL"))) %>%
  arrange(Facility.Hospital.Id_Worked, Payroll.Name,
          as.Date(End.Date, "%m/%d/%Y")) %>%
  mutate(Hours = prettyNum(Hours, big.mark = ",")) %>%
  pivot_wider(names_from = End.Date, values_from = Hours)

View(piv_wide_check)
View(piv_wide_check_prev)

## JC update in DB reminder -------------------------------------------------

showDialog(
  title = "Update JC Mapping in DB",
  message = paste0(
    "USER WARNING: ",
    "Before proceeding, ",
    "it's best to run the sftp_sync_decrypt_insert script on R Connect to",
    "identify new JCs.",
    "Then populate the JC info in the OAO_DEVELOPMENT Schema and run the ",
    "sftp_sync_decrypt_insert script on R Connect again to ensure",
    "the PROD table is updated.")
)

## Data  --------------------------------------------------------------------
row_count <- nrow(raw_payroll)

if ("Job.Code.Description" %in% colnames(raw_payroll)) {
  bislr_payroll <- raw_payroll %>%
    rename(Position.Code.Description = Job.Code.Description)
} else {
  bislr_payroll <- raw_payroll
}

bislr_payroll <- bislr_payroll %>%
  mutate(DPT.WRKD = paste0(substr(Full.COA.for.Worked, 1, 3),
                           substr(Full.COA.for.Worked, 41, 44),
                           substr(Full.COA.for.Worked, 5, 7),
                           substr(Full.COA.for.Worked, 12, 16)),
         DPT.HOME = paste0(substr(Full.COA.for.Home, 1, 3),
                           substr(Full.COA.for.Home, 41, 44),
                           substr(Full.COA.for.Home, 5, 7),
                           substr(Full.COA.for.Home, 12, 16)),
         DPT.WRKD.LEGACY = paste0(substr(Reverse.Map.for.Worked, 1, 4),
                                  substr(Reverse.Map.for.Worked, 13, 14),
                                  substr(Reverse.Map.for.Worked, 16, 19)),
         DPT.HOME.LEGACY = paste0(substr(Reverse.Map.for.Home, 1, 4),
                                  substr(Reverse.Map.for.Home, 13, 14),
                                  substr(Reverse.Map.for.Home, 16, 19)),
         Start.Date = as.Date(Start.Date, format = "%m/%d/%Y"),
         End.Date = as.Date(End.Date, format = "%m/%d/%Y"),
         Employee.Name = iconv(substr(Employee.Name, 1, 30),
                               from = "UTF-8",
                               to = "ASCII//TRANSLIT"),
         Approved.Hours.per.Pay.Period = round(Approved.Hours.per.Pay.Period,
                                               digits = 0),
         Job.Code = str_trim(Job.Code),
         Position.Code.Description = str_trim(Position.Code.Description)) %>%
  mutate(
    DPT.WRKD = case_when(
      trimws(Department.Name.Worked.Dept) == "" ~
        as.character(Department.IdWHERE.Worked),
      TRUE ~ DPT.WRKD),
    DPT.HOME = case_when(
      trimws(Department.Name.Home.Dept) == "" ~
        as.character(Department.ID.Home.Department),
      TRUE ~ DPT.HOME)) %>%
  mutate(
    DPT.WRKD = case_when(
      WD_Department %in% cc_fundnum_conv ~ WD_Fund_number,
      TRUE ~ DPT.WRKD)) %>%
  # for the future after LPM team has requested that the Home Fund Number
  # be populated in the Full.COA.for.Home string:
  # mutate(
  #   DPT.HOME = case_when(
  #     HD_Department %in% cc_fundnum_conv ~ substr(Full.COA.for.Home, 25, 35),
  #     TRUE ~ DPT.HOME)) %>%
  mutate(
    Job.Code = case_when(
      paste0(DPT.WRKD, "-", toupper(Employee.Name)) %in%
        paste0(msus_removal_list$`Department IdWHERE Worked`, "-",
               msus_removal_list$`Employee Name`) ~
        unique(msus_removal_list$`New Job Code`),
      TRUE ~ Job.Code),
    Position.Code.Description = case_when(
      paste0(DPT.WRKD, "-", toupper(Employee.Name)) %in%
        paste0(msus_removal_list$`Department IdWHERE Worked`, "-",
               msus_removal_list$`Employee Name`) ~
        unique(msus_removal_list$`New Job Code Description`),
      TRUE ~ Position.Code.Description)) %>%
  left_join(pay_cycles_uploaded) %>%
  left_join(map_uni_jobcodes %>%
              filter(PAYROLL == "BISLR") %>%
              select(J.C, PROVIDER, JC_in_UniversalFile) %>%
              rename(Job.Code = J.C)) %>%
  left_join(map_uni_paycodes %>%
              select(RAW.PAY.CODE, Paycode_in_Universal) %>%
              rename(Pay.Code = RAW.PAY.CODE)) %>%
  left_join(dict_premier_dpt %>%
              select(Site, Cost.Center, Dpt_in_Dict) %>%
              rename(Home.FacilityOR.Hospital.ID = Site,
                     DPT.HOME = Cost.Center,
                     HomeDpt_in_Dict = Dpt_in_Dict)) %>%
  left_join(dict_premier_dpt %>%
              select(Site, Cost.Center, Dpt_in_Dict) %>%
              rename(Facility.Hospital.Id_Worked = Site,
                     DPT.WRKD = Cost.Center,
                     WRKDpt_in_Dict = Dpt_in_Dict)) %>%
  left_join(map_premier_jobcode %>%
              select(Site, Job.Code, Cost.Center.Code, JC_in_Dict) %>%
              rename(Home.FacilityOR.Hospital.ID = Site,
                     HOMEJC_in_Dict = JC_in_Dict),
            by = c("Home.FacilityOR.Hospital.ID" = "Home.FacilityOR.Hospital.ID",
                   "Job.Code" = "Job.Code",
                   "DPT.HOME" = "Cost.Center.Code")) %>%
  left_join(map_premier_jobcode %>%
              select(Site, Job.Code, Cost.Center.Code, JC_in_Dict) %>%
              rename(Facility.Hospital.Id_Worked = Site,
                     WRKJC_in_Dict = JC_in_Dict),
            by = c("Facility.Hospital.Id_Worked" = "Facility.Hospital.Id_Worked",
                   "Job.Code" = "Job.Code",
                   "DPT.WRKD" = "Cost.Center.Code")) %>%
  mutate(Job.Code_up = substr(Job.Code, 1, 10),
         Approved.Hours.per.Pay.Period = case_when(
           is.na(Approved.Hours.per.Pay.Period) ~ 0,
           TRUE ~ Approved.Hours.per.Pay.Period))

accrual_raw_detail <- bislr_payroll %>%
  filter(DPT.WRKD.LEGACY %in%
    subset(report_list, Report.ID %in% accrual_report_ids)$Cost.Center)

bislr_payroll <- bislr_payroll %>%
  mutate(
    DPT.WRKD = case_when(
      DPT.WRKD.LEGACY %in%
        subset(report_list, Report.ID %in% accrual_report_ids)$Cost.Center &
        !Department.Name.Worked.Dept %in% true_accrual_cc_desc ~
          DPT.WRKD.LEGACY,
      TRUE ~ DPT.WRKD),
    Department.Name.Worked.Dept = case_when(
      DPT.WRKD.LEGACY %in%
        subset(report_list, Report.ID %in% accrual_report_ids)$Cost.Center &
        !Department.Name.Worked.Dept %in% true_accrual_cc_desc ~
          "ACCRUAL COST CENTER",
      TRUE ~ Department.Name.Worked.Dept),
    WRKDpt_in_Dict = case_when(
      DPT.WRKD %in%
        subset(report_list, Report.ID %in% accrual_report_ids)$Cost.Center ~ 1,
      TRUE ~ WRKDpt_in_Dict),
    WRKJC_in_Dict = case_when(
      DPT.WRKD %in%
        subset(report_list, Report.ID %in%
                 accrual_report_ids)$Cost.Center ~ NA_real_,
      TRUE ~ WRKJC_in_Dict)
    )

if (nrow(bislr_payroll) != row_count) {
  showDialog(title = "Join error",
             message = paste("Row count failed at", "bislr_payroll"))
  stop(paste("Row count failed at", "bislr_payroll"))
}

## Update Universal Files --------------------------------------------------
loop <- 0
while (NA %in% unique(bislr_payroll$JC_in_UniversalFile) |
       NA %in% unique(bislr_payroll$PROVIDER)) {
  loop <- loop + 1
  new_jobcodes <- bislr_payroll %>%
    filter(is.na(JC_in_UniversalFile)) %>%
    select(Job.Code, Position.Code.Description) %>%
    unique() %>%
    mutate(JobDescCap = toupper(Position.Code.Description))

  row_count <- nrow(new_jobcodes)
  new_jobcodes <- new_jobcodes %>%
    left_join(map_uni_jobcodes %>%
      filter(PAYROLL == "MSHQ") %>%
      select(J.C.DESCRIPTION, PROVIDER, PREMIER.J.C,
             PREMIER.J.C.DESCRIPTION) %>%
      rename(JobDescCap = J.C.DESCRIPTION)) %>%
    select(-JobDescCap) %>%
    unique()
  if (nrow(new_jobcodes) != row_count) {
    showDialog(title = "Join error",
               message = paste("Row count failed at", "new_jobcodes"))
    stop(paste("Row count failed at", "new_jobcodes"))
  }

  View(new_jobcodes)


  if (nrow(new_jobcodes) > 0) {
    new_jc_path <- paste0(dir_universal, "/Mapping/BISLR payroll script")
    write.csv(new_jobcodes,
              paste0(new_jc_path,
                     "/new_jc_for_Universal_File_",
                     as.character(Sys.time(), format = "%Y-%m-%d"),
                     if (loop == 1) {
                       ""
                     }else{
                       paste0("_V", loop)
                     },
                     ".csv"))
  }

  # MM: nothing can be navigated and viewed in an R session while
  # the following window is open.  Perhaps this is an instance to
  # add an additional step to get user input in the console
  # or find another strategy
  warn_new_jc <-
    showQuestion(title = "Warning",
                 message =
                   paste0(
                     if (loop == 1) {
                       paste0("Job Code attention needed!\n",
                              "New jobcodes and/or PROVIDER value is NA.\n")
                     } else {
                       paste0("There are still new job codes and/or \n",
                              "PROVIDER values of NA.\n")
                     },
                     "Update Universal Job Code DB Table before continuing. \n",
                     "\nIf there are new Job Codes they were written to a ",
                     "file in this directory:\n'", new_jc_path, "'\n\n",
                     "\n Has the mapping Production DB Table been completely updated?",
                     "\n \n If you want to quit running the code select NO \n",
                     " then click the stop code button in the console"),
                 ok = "YES", cancel = "NO")

  if (warn_new_jc == FALSE) {
    Sys.sleep(15)
  }

  # MM: I believe Anjelica and I discussed nesting many of these statements
  # within an if() statement based on an "ok" user input from the previous
  # prompt, but there's probably a simpler solution
  
  # in order to make udpates you must:
  # 1. add info to the Development Table
  # 2. run the the sftp_sync_descrypt_insert Document on RStudio Connect
  #    (by using the refresh icon on the Document page)
  #    to get updates on the Dev Table into the Production table
  map_uni_jobcodes <- tbl(oao_con, "LPM_MAPPING_JOBCODE") %>%
    collect()
  map_uni_jobcodes <- map_uni_jobcodes %>%
    rename(J.C = JOBCODE,
           PAYROLL = PAYROLL,
           J.C.DESCRIPTION = JOBCODE_DESCRIPTION,
           PROVIDER = PROVIDER,
           PREMIER.J.C = JOBCODE_PREMIER,
           PREMIER.J.C.DESCRIPTION = JOBCODE_PREMIER_DESCRIPTION) %>%
    mutate(J.C = str_trim(J.C)) %>%
    mutate(JC_in_UniversalFile = 1)

  row_count <- nrow(bislr_payroll)
  bislr_payroll <- left_join(
    bislr_payroll %>%
      select(-JC_in_UniversalFile, -PROVIDER),
    map_uni_jobcodes %>%
      filter(PAYROLL == "BISLR") %>%
      select(J.C, PROVIDER, JC_in_UniversalFile) %>%
      rename(Job.Code = J.C))
  if (nrow(bislr_payroll) != row_count) {
    showDialog(title = "Join error",
               message = paste("Row count failed at",
                               "bislr_payroll new job codes"))
    stop(paste("Row count failed at", "bislr_payroll new job codes"))
  }
  Sys.sleep(2)
}

loop <- 0
while (NA %in% unique(bislr_payroll$Paycode_in_Universal)) {
  loop <- loop + 1
  new_paycodes <- bislr_payroll %>%
    filter(is.na(Paycode_in_Universal)) %>%
    select(Facility.Hospital.Id_Worked, Pay.Code) %>%
    unique()
  View(new_paycodes)

  write.csv(new_paycodes,
            paste0(new_jc_path,
                   "/new_paycodes_for_Universal_File_",
                   as.character(Sys.time(), format = "%Y-%m-%d"),
                   if (loop == 1) {
                     ""
                   } else {
                     paste0("_V", loop)
                   },
                   ".csv"))

  warn_new_pc <-
    showQuestion(
      title = "Warning",
      message = paste0(
        if (loop == 1) {
          "New pay codes detected! \n"

          if (max(nchar(new_paycodes$Pay.Code)) > char_len_paycode) {
            paste0("\nBe aware a paycode will need to be shortened so it's ",
                   "not longer than the ", char_len_paycode, " limit.\n")
          }

        } else {
          "There are still new pay codes. \n"

          if (max(nchar(new_paycodes$Pay.Code)) > char_len_paycode) {
            paste0("\nBe aware a paycode will need to be shortened so it's ",
                   "not longer than the ", char_len_paycode, " character ",
                   "limit.\n")
          }
        },
        "Update Universal Pay Code DB Table before continuing. \n",
        "\n Have new pay codes been added?",
        "\n \n If you want to quit running the code select No \n",
        " then click the stop code button in the console"),
      ok = "Yes", cancel = "No")

  if (warn_new_pc == FALSE) {
    Sys.sleep(15)
  }

  # in order to make udpates you must:
  # 1. add info to the Development Table
  # 2. run the the sftp_sync_descrypt_insert Document on RStudio Connect
  #    (by using the refresh icon on the Document page)
  #    to get updates on the Dev Table into the Production table
  map_uni_paycodes <- tbl(oao_con, "LPM_MAPPING_PAYCODE") %>%
    collect()
  map_uni_paycodes <- map_uni_paycodes %>%
    rename(RAW.PAY.CODE = PAYCODE_RAW,
           PAY.CODE = PAYCODE_PREMIER,
           PAY.CODE.NAME = PAYCODE_DESCRIPTION,
           PAY.CODE.CATEGORY = PAYCODE_CATEGORY,
           INCLUDE.HOURS = INCLUDE_HOURS,
           INCLUDE.EXPENSES = INCLUDE_EXPENSES,
           WORKED.PAY.CODE = WORKED_PAYCODE) %>%
    mutate(Paycode_in_Universal = 1)

  row_count <- nrow(bislr_payroll)

  bislr_payroll <- left_join(
    bislr_payroll %>%
      select(-Paycode_in_Universal),
    map_uni_paycodes %>%
      select(RAW.PAY.CODE, Paycode_in_Universal) %>%
      rename(Pay.Code = RAW.PAY.CODE))
  if (nrow(bislr_payroll) != row_count) {
    showDialog(
      title = "Join error",
      message = paste("Row count failed at", "bislr_payroll new pay codes"))
    stop(paste("Row count failed at", "bislr_payroll new pay codes"))
  }
  Sys.sleep(2)
}

## Paycycles --------------------------------------------------------------

# Paycycles to filter on

# based on the data
paycycles_data <- bislr_payroll %>%
  select(Start.Date, End.Date) %>%
  distinct() %>%
  filter(as.Date(Start.Date, "%m/%d/%Y") > dist_prev &
           as.Date(Start.Date, "%m/%d/%Y") < distribution_date)

# based on Pay_Cycle_uploaded
filter_dates <- bislr_payroll %>%
  filter(is.na(Pay_Cycle_Uploaded)) %>%
  select(Start.Date, End.Date) %>%
  unique() %>%
  arrange(Start.Date) %>%
  filter(
    End.Date > dist_prev,
    !Start.Date > distribution_date) %>%
  mutate(upload_date = 1) %>%
  arrange(Start.Date, End.Date)

# Updating pay cycles filter dates dictionary
if (nrow(filter_dates) > 0) {
  filter_dates_refresh <- filter_dates %>%
    select(-upload_date) %>%
    mutate(capture_time = as.character(Sys.time()))
  
  pay_cycles_uploaded_raw <- pay_cycles_uploaded_raw %>%
    mutate(Start.Date = as.Date(Start.Date),
           End.Date = as.Date(End.Date))
  
  pay_cycles_upload_refresh <- rbind(pay_cycles_uploaded_raw,
                                     filter_dates_refresh)
  date_filtering <- filter_dates
}else{
  showDialog(
    title = "Dates in Data",
    message = paste("It appears that you're working with data",
                    "that has already been prepared for Premier upload",
                    "at some point in the past.  Be careful not to",
                    "overwrite data files by mistake."))
  date_filtering <- paycycles_data %>%
    mutate(upload_date = 1) %>%
    arrange(Start.Date, End.Date)
}

## JC ID check ----------------------------------------------------
# this section is here because if any job codes become duplicates after
# getting shortened then we need to be aware of the conflict and resolve it
# This might warrant a restructuring of the JC universal mapping file

# There's an assumption that the universal jc mapping file has been updated
# with new jobcodes by this point.

# incorporate this into the if-statement above looking at JC_in_UniversalFile?
if (exists("new_jobcodes")) {
  jc_check_new_long <- new_jobcodes %>%
    mutate(Job.Code_up = substr(Job.Code, 1, 10)) %>%
    inner_join(dict_premier_jobcode %>%
                 filter(Site %in% c("630571", "NY2162", "NY2163")) %>%
                 select(Job.Code) %>%
                 distinct(),
               by = c("Job.Code_up" = "Job.Code"))

  # This can get incorporated into Preprocessing > Update Universal Files
  # section

  if (nrow(jc_check_new_long) > 0) {
    showDialog(title = "ERROR: Job Codes",
               message = paste0("There are duplicates in ",
                                "shortened Job Codes.  ",
                                "These will require special handling.  ",
                                "Please stop the script and resolve concerns ",
                                "before restarting."))
    stop(paste0("There are duplicates in shortened Job Codes.\n\n",
                "These will require special handling.\n\n",
                "Please stop the script and resolve concerns ",
                "before restarting.\n"))
    View(jc_check_new_long)
  } else {
    cat("All shortened job codes are unique\n")
  }
}

# Formatting Outputs ---------------------------------------------------------

## Premier Payroll File ----------------------------------------------------

row_count <- nrow(bislr_payroll)
upload_payroll <- bislr_payroll %>%
  # best known alternative to a join when trying to match on multiple columns is
  # a case_when and mutate on multiple columns
  # This is kind of like creating a helper column in Excel
  # DRAFT of alternative to join:
  # mutate(upload_date = case_when(
  #   paste0(Start.Date, "-", End.Date) %in%
  #     paste0(filter_dates$Start.Date, "-", filter_dates$End.Date) ~ 1,
  #   TRUE ~ )) %>%
  # join seems simple/quick enough that we could keep it.
  left_join(date_filtering) %>%
  left_join(select(map_uni_paycodes, RAW.PAY.CODE, PAY.CODE),
            by = c("Pay.Code" = "RAW.PAY.CODE")) %>%
  mutate(Pay.Code = PAY.CODE,
         PAY.CODE = NULL,
         Start.Date = format(Start.Date, "%m/%d/%Y"),
         End.Date = format(End.Date, "%m/%d/%Y"))

if (nrow(upload_payroll) != row_count) {
  showDialog(title = "Join error",
             message = paste("Row count failed at", "upload_payroll"))
  stop(paste("Row count failed at", "upload_payroll"))
}

upload_payroll <- upload_payroll %>%
  filter(PROVIDER == 0) %>%
  filter(!is.na(upload_date)) %>%
  group_by(PartnerOR.Health.System.ID, Home.FacilityOR.Hospital.ID, DPT.HOME,
    Facility.Hospital.Id_Worked, DPT.WRKD, Start.Date, End.Date,
    Employee.ID, Employee.Name, Approved.Hours.per.Pay.Period,
    Job.Code_up, Pay.Code) %>%
  summarize(Hours = round(sum(Hours, na.rm = TRUE), 4),
            Expense = round(sum(Expense, na.rm = TRUE), 2)) %>%
  ungroup()

# upload_payroll will be split into BIB and SLW in the Exporting section

## Premier Reference Files -------------------------------------------------

### Dpt Dict and Map -------------------------------------------------------

if (NA %in% bislr_payroll$HomeDpt_in_Dict |
    NA %in% bislr_payroll$WRKDpt_in_Dict) {

  # update dpt dict
  upload_dict_dpt <- rbind(
    bislr_payroll %>%
      filter(is.na(HomeDpt_in_Dict)) %>%
      select(PartnerOR.Health.System.ID, Home.FacilityOR.Hospital.ID,
             DPT.HOME, Department.Name.Home.Dept) %>%
      setNames(colnames(dict_premier_dpt %>%
                          select(-Dpt_in_Dict))),
    bislr_payroll %>%
      filter(is.na(WRKDpt_in_Dict)) %>%
      select(PartnerOR.Health.System.ID, Facility.Hospital.Id_Worked,
             DPT.WRKD, Department.Name.Worked.Dept) %>%
      setNames(colnames(dict_premier_dpt %>%
                          select(-Dpt_in_Dict)))) %>%
    # check for special characters in name (e.g. ampersand &)
    # mutate(Cost.Center.Description = case_when(
    #   str_detect(Cost.Center.Description, "&") ~
    #     str_replace(Cost.Center.Description, "&", "AND"),
    #   TRUE ~ Cost.Center.Description)) %>%
    # check for cost center name length
    mutate(Cost.Center.Description =
             str_sub(Cost.Center.Description, 1, 50)) %>%
    distinct()
  # if there are no new depts, then this will be empty


  # update dpt map

  row_count <- nrow(upload_dict_dpt)
  upload_map_dpt <- upload_dict_dpt %>%
    left_join(dict_premier_dpt)
  if (nrow(upload_map_dpt) != row_count) {
    showDialog(title = "Join error",
               message = paste("Row count failed at", "upload_map_dpt"))
    stop(paste("Row count failed at", "upload_map_dpt"))
  }

  upload_map_dpt <- upload_map_dpt %>%
    filter(is.na(Dpt_in_Dict)) %>%
    mutate(Dpt_in_Dict = NULL,
           Cost.Center.Description = NULL) %>%
    mutate(effective_date = format(map_effective_date_dpt, "%m/%d/%Y"),
           prem_map = new_dpt_map) %>%
    relocate(effective_date, .before = Corporation.Code) %>%
    distinct()
}

### Dpt JC Dict and Map -----------------------------------------------------

if (NA %in% bislr_payroll$WRKJC_in_Dict |
    NA %in% bislr_payroll$HOMEJC_in_Dict) {

  # update dpt job code dict
  upload_dict_dpt_jc <- rbind(
    bislr_payroll %>%
      filter(is.na(WRKJC_in_Dict), PROVIDER == 0) %>%
      select(PartnerOR.Health.System.ID, Facility.Hospital.Id_Worked,
             Job.Code_up, Position.Code.Description, DPT.WRKD) %>%
      setNames(c(colnames(dict_premier_jobcode %>%
                          select(-JC_in_Dicty)), "Cost.Center")),
    bislr_payroll %>%
      filter(is.na(HOMEJC_in_Dict), PROVIDER == 0) %>%
      select(PartnerOR.Health.System.ID, Home.FacilityOR.Hospital.ID,
             Job.Code_up, Position.Code.Description, DPT.HOME) %>%
      setNames(c(colnames(dict_premier_jobcode %>%
                            select(-JC_in_Dicty)), "Cost.Center"))) %>%
    # for the future, we might look out for handling descriptions
    # that have special characters, such as & (ampersand)
    # mutate(Job.Code.Description = case_when(
    #   str_detect(Job.Code.Description, "&") ~
    #     str_replace(Job.Code.Description, "&", "AND"),
    #   TRUE ~ Job.Code.Description)) %>%
    mutate(Job.Code.Description =
             str_trim(str_sub(Job.Code.Description, 1, 50))) %>%
    distinct(across(-Job.Code.Description), .keep_all = TRUE)

  # update dpt job code map

  row_count <- nrow(upload_dict_dpt_jc)
  upload_map_dpt_jc <- upload_dict_dpt_jc %>%
    select(-Job.Code.Description) %>%
    left_join(map_uni_jobcodes %>%
                filter(PAYROLL == "BISLR") %>%
                select(J.C, PREMIER.J.C) %>%
                mutate(J.C.prem = substr(J.C, 1, 10)) %>%
                select(-J.C),
              by = c("Job.Code" = "J.C.prem")) %>%
    mutate(effective_date = format(map_effective_date_dpt, "%m/%d/%Y"),
           alloc_pct = jc_alloc_pct) %>%
    relocate(effective_date, .before = Corporation.Code) %>%
    distinct()
  if (nrow(upload_map_dpt_jc) != row_count) {
    showDialog(
      title = "Join error",
      message = paste("Row count failed at", "upload_map_dpt_jc"))
    stop(paste("Row count failed at", "upload_map_dpt_jc"))
  }

  # FYI check:
  # This can be moved into the QC section and/or we can filter out any NA
  # PREMIER.JC rows and handle them manually
  upload_map_dpt_jc_na <- upload_map_dpt_jc %>%
    filter(is.na(PREMIER.J.C))
  View(upload_map_dpt_jc_na)
}

### Paycode Dict and Map ---------------------------------------------------


premier_missing_paycode <- anti_join(upload_payroll %>%
                                       select(Pay.Code) %>%
                                       distinct(),
                                     dict_premier_paycode %>%
                                       select(Pay.Code))

if (nrow(premier_missing_paycode) > 0) {

  # update paycode dict
  upload_dict_paycode <- premier_missing_paycode %>%
    left_join(map_uni_paycodes %>%
                select(PAY.CODE, PAY.CODE.NAME),
              by = c("Pay.Code" = "PAY.CODE")) %>%
    merge(unique(upload_payroll$Facility.Hospital.Id_Worked)) %>%
    rename(Site = y) %>%
    mutate(Corp = corp_code) %>%
    relocate(c(Corp, Site), .before = Pay.Code)

  # user should be warned if character limit is reached
  if (max(nchar(upload_dict_paycode$Pay.Code)) > char_len_paycode |
    max(nchar(upload_dict_paycode$PAY.CODE.NAME)) > char_len_paycode_name) {
    showDialog(
      title = "Paycode field error",
      message = paste0(
        "Either a paycode or paycode name has more ",
        "characters than permitted.  ",
        "Please fix them in the upload file to prevent upload errors ",
        "and in the Universal Mapping file."))
  }

  # update paycode map

  row_count <- nrow(upload_dict_paycode)
  upload_map_paycode <- upload_dict_paycode %>%
    select(-PAY.CODE.NAME) %>%
    left_join(map_uni_paycodes %>%
                select(PAY.CODE, PAY.CODE.CATEGORY, INCLUDE.HOURS,
                       INCLUDE.EXPENSES),
              by = c("Pay.Code" = "PAY.CODE")) %>%
    mutate(eff_date = as.character(dist_prev - lubridate::days(6), "%m/%d/%Y"),
           alloc_pct = 100) %>%
    relocate(eff_date, .before = Corp)

  if (nrow(upload_map_paycode) != row_count) {
    showDialog(title = "Join error",
               message = paste("Row count failed at", "upload_map_paycode"))
    stop(paste("Row count failed at", "upload_map_paycode"))
  }
}


# Quality Checks -------------------------------------------------------


## JC with multiple Description -------------------------------------------

# this should now be unneeded because of the udpate for
# 1-to-1 JC ID to JC Description

jc_desc_check <- bislr_payroll %>%
  select(Job.Code, Position.Code.Description) %>%
  distinct() %>%
  group_by(Job.Code) %>%
  summarize(freq = n()) %>%
  arrange(-freq, Job.Code) %>%
  # filter(freq > 1) %>%
  inner_join(bislr_payroll %>%
    select(Job.Code, Position.Code.Description, PROVIDER) %>%
    distinct())

# 5 is the selected threshold because of the DUS_RMV jobcode
if (max(jc_desc_check$freq) > jc_desc_threshold) {
  showDialog(
    title = "Jobcode Description Check",
    message = paste(
      "There are a large number of descriptions",
      "mapped to the same jobcode. ",
      "Review the jobcodes with multiple",
      "descriptions before proceeding."
    ))
  View(jc_desc_check)
}
# pull in Premier jobcode category or other info to help?
# this will be infrequent, so it's not be worth coding for it at
# this point in time


## cost center description changes -----------------------------------------

dept_desc <- bislr_payroll %>%
  select(Department.IdWHERE.Worked, Department.Name.Worked.Dept) %>%
  mutate(Department.IdWHERE.Worked =
           as.character(Department.IdWHERE.Worked)) %>%
  distinct()

dept_desc_check <- full_join(dict_premier_dpt, dept_desc,
                             by = c("Cost.Center" =
                                      "Department.IdWHERE.Worked")) %>%
  # may want to include NY0014 in the future to go ahead and update, too
  filter(Site %in% c("630571", "NY2162", "NY2163")) %>%
  filter(!is.na(Cost.Center.Description)) %>%
  filter(!is.na(Department.Name.Worked.Dept)) %>%
  mutate(desc_same =
           (Cost.Center.Description == Department.Name.Worked.Dept)) %>%
  mutate(dict_desc_length = nchar(Cost.Center.Description)) %>%
  filter(desc_same == FALSE) %>%
  filter(Department.Name.Worked.Dept != "ACCRUAL COST CENTER") %>%
  rename(Premier_Dict_Cost_Ctr_Desc = Cost.Center.Description,
         Current_Payroll_Cost_Ctr_Desc = Department.Name.Worked.Dept)

if (nrow(dept_desc_check > 0)) {
  showDialog(
    title = "Cost Center Description Check",
    message = paste(
      "There are Cost Center Descriptions ",
      "that appear to have changed.  Review for changes in operations ",
      "and update in Premier and DB."
    ))
  View(dept_desc_check)
}

## cost center and upload FTE count -------------------------------------

# average FTEs since previous distribution

## FYI: this is based on the upload file, so it only includes providers

# need Pay Code info to get appropriate hours
row_count <- nrow(upload_payroll)
fte_summary <- upload_payroll %>%
  left_join(map_uni_paycodes %>%
    select(PAY.CODE, INCLUDE.HOURS, WORKED.PAY.CODE) %>%
    distinct(),
  by = c("Pay.Code" = "PAY.CODE"))

if (nrow(fte_summary) != row_count) {
  showDialog(title = "Join error",
             message = paste("Row count failed at", "fte_summary"))
  stop(paste("Row count failed at", "fte_summary"))
}

fte_summary <- fte_summary %>%
  filter(INCLUDE.HOURS == 1) %>%
  mutate(Hours_Worked = WORKED.PAY.CODE * Hours) %>%
  group_by(Facility.Hospital.Id_Worked, DPT.WRKD) %>%
  summarize(
    Avg_FTEs_worked =
      round(sum(Hours_Worked, na.rm = TRUE) /
              (37.5 * (as.numeric(distribution_date - dist_prev) / 7)), 1),
    Avg_FTEs_paid =
      round(sum(Hours, na.rm = TRUE) /
              (37.5 * (as.numeric(distribution_date - dist_prev) / 7)), 1)) %>%
  ungroup() %>%
  # removing the formatting of this date could eliminate some need
  # for all the date processing within this coding section
  mutate(dist_date = format(distribution_date, "%m/%d/%Y")) %>%
  relocate(dist_date, .before = Avg_FTEs_worked) %>%
  mutate(capture_time = as.character(Sys.time())) %>%
  rename(Site = Facility.Hospital.Id_Worked, Department = DPT.WRKD) %>%
  mutate(Site = case_when(
    Site == "630571" ~ "MSBIB",
    Site == "NY2162" ~ "MSW",
    Site == "NY2163" ~ "MSM",
    TRUE ~ "Other"))

fte_summary_path <- paste0("/SharedDrive/deans/Presidents/",
                           "SixSigma/MSHS Productivity/Productivity/",
                           "Labor - Data/Multi-site/BISLR/Quality Checks/",
                           "Source Data/")

fte_summary <- rbind(fte_summary,
                     read_xlsx(path = paste0(fte_summary_path,
                                              "fte_summary.xlsx"),
                                col_types = c(rep("text", 8),
                                               rep("numeric", 2),
                                               "text"),
                                sheet = "fte_summary") %>%
                       select(Site, Department, dist_date,
                              Avg_FTEs_worked, Avg_FTEs_paid,
                              capture_time))

if (fte_summary %>% select(dist_date) %>% distinct() %>% nrow() !=
    fte_summary %>% select(dist_date, capture_time) %>% distinct() %>% nrow()) {
  showDialog(title = "Warning",
             message = paste(
               "You will likely be appending rows to",
               "the fte_summary file for a distribution",
               "period that already exists in the file."))
  stop(paste("You will likely be appending rows to",
             "the fte_summary file for a distribution",
             "period that already exists in the file."))
}

row_count <- nrow(fte_summary)
fte_summary <- fte_summary %>%
  left_join(map_uni_reports %>%
              # try this without limiting to Dept Breakdown so other
              # Rep Def can be indicated in the fte_summary even
              # if a report has been turned off or isn't included in
              # the Dept Breakdown (e.g. Small Dept Def)
              filter(is.na(CLOSED)) %>% # & DEPARTMENT.BREAKDOWN == 1) %>%
              select(DEFINITION.CODE, DEFINITION.NAME, ORACLE.COST.CENTER,
                     CORPORATE.SERVICE.LINE, VP) %>%
              distinct(),
            by = c("Department" = "ORACLE.COST.CENTER")) %>%
  left_join(rbind(
    dict_premier_dpt %>%
      select(-Dpt_in_Dict) %>%
      mutate(Site = case_when(
        Site == "630571" ~ "MSBIB",
        Site == "NY2162" ~ "MSW",
        Site == "NY2163" ~ "MSM",
        TRUE ~ "Other")),
    upload_dict_dpt %>% # need to consider when there's no new depts to upload
      mutate(Site = case_when(
        Site == "630571" ~ "MSBIB",
        Site == "NY2162" ~ "MSW",
        Site == "NY2163" ~ "MSM",
        TRUE ~ "Other"))) %>%
      select(-Corporation.Code),
    by = c("Site" = "Site", "Department" = "Cost.Center")) %>%
  select(Site, Department, Cost.Center.Description,
         DEFINITION.CODE, DEFINITION.NAME,
         CORPORATE.SERVICE.LINE, VP, dist_date,
         Avg_FTEs_worked, Avg_FTEs_paid, capture_time)

# some Cost.Center.Description values are showing as NA
# - this is minimal and doesn't look worth pursuing at this time
  
if (nrow(fte_summary) != row_count) {
  showDialog(title = "Join error",
             message = paste("Row count failed at", "fte_summary"))
  stop(paste("Row count failed at", "fte_summary"))
}

# filtering so only the trailing year is included
fte_summary <- fte_summary %>%
  filter(as.numeric(as.Date(distribution_date) -
                      as.Date(dist_date, "%m/%d/%Y")) < 390)


### wide summary ------------------------------------------------------------

# get the minimum date for each cost center
fte_summary_cc_age <- fte_summary %>%
  select(Site, Department, dist_date) %>%
  mutate(dist_date = as.Date(dist_date, "%m/%d/%Y")) %>%
  arrange(dist_date, Site, Department) %>%
  distinct(across(-dist_date), .keep_all = TRUE) %>%
  rename(oldest_date = dist_date)

# PAID

fte_summary_wide_paid <- fte_summary %>%
  mutate(dist_date = as.Date(dist_date, "%m/%d/%Y")) %>%
  arrange(dist_date, Site, Department) %>%
  select(-Avg_FTEs_worked, -capture_time) %>%
  pivot_wider(names_from = dist_date, values_from = Avg_FTEs_paid,
              values_fill = 0)

fte_summary_wide_paid$current_change_raw <-
  pull(select(fte_summary_wide_paid,
              contains(format(distribution_date, "%Y-%m-%d")))) -
  pull(select(fte_summary_wide_paid,
              contains(format(dist_prev, "%Y-%m-%d"))))

fte_summary_wide_paid$current_change_pct <-
  formattable::percent(
    fte_summary_wide_paid$current_change_raw /
      pull(select(fte_summary_wide_paid,
                  contains(format(dist_prev, "%Y-%m-%d")))), digits = 1)

fte_summary_wide_paid <- fte_summary_wide_paid %>%
  left_join(fte_summary_cc_age) %>%
  arrange(oldest_date)

fte_summary_extreme_change_pd <- fte_summary_wide_paid %>%
  filter(current_change_raw >= 3 | current_change_raw <= -3) %>%
  arrange(-current_change_raw)

View(fte_summary_extreme_change_pd)

fte_summary_new_dept_pd <- fte_summary_wide_paid %>%
  filter(oldest_date == distribution_date)

View(fte_summary_new_dept_pd)

# WORKED

fte_summary_wide_worked <- fte_summary %>%
  mutate(dist_date = as.Date(dist_date, "%m/%d/%Y")) %>%
  arrange(dist_date, Site, Department) %>%
  select(-Avg_FTEs_paid, -capture_time) %>%
  pivot_wider(names_from = dist_date, values_from = Avg_FTEs_worked,
              values_fill = 0)

fte_summary_wide_worked$current_change_raw <-
  pull(select(fte_summary_wide_worked,
              contains(format(distribution_date, "%Y-%m-%d")))) -
  pull(select(fte_summary_wide_worked,
              contains(format(dist_prev, "%Y-%m-%d"))))

fte_summary_wide_worked$current_change_pct <-
  formattable::percent(
    fte_summary_wide_worked$current_change_raw /
      pull(select(fte_summary_wide_worked,
                  contains(format(dist_prev, "%Y-%m-%d")))), digits = 1)

fte_summary_wide_worked <- fte_summary_wide_worked %>%
  left_join(fte_summary_cc_age) %>%
  arrange(oldest_date)

fte_summary_extreme_change_wrk <- fte_summary_wide_worked %>%
  filter(current_change_raw >= 3 | current_change_raw <= -3) %>%
  arrange(-current_change_raw)

View(fte_summary_extreme_change_wrk)

fte_summary_new_dept_wrk <- fte_summary_wide_worked %>%
  filter(oldest_date == distribution_date)

View(fte_summary_new_dept_wrk)


## 8600 Accrual Site Summary --------------------------------------------

accrual_summary <- bislr_payroll %>%
  left_join(date_filtering) %>%
  filter(upload_date == 1) %>%
  filter(PROVIDER == 0) %>%
  filter(DPT.WRKD %in%
           subset(report_list,
                  Report.ID %in% accrual_report_ids)$Cost.Center) %>%
  group_by(Facility.Hospital.Id_Worked, DPT.WRKD, Start.Date, End.Date) %>%
  summarize(Hours = sum(Hours, na.rm = TRUE),
            Expense = sum(Expense, na.rm = TRUE)) %>%
  ungroup()

# row count check not required for joining date_filtering
accrual_raw_detail <- accrual_raw_detail %>%
  left_join(date_filtering) %>%
  filter(upload_date == 1)

row_count <- nrow(accrual_raw_detail)
accrual_raw_detail <- accrual_raw_detail %>%
  left_join(map_uni_paycodes %>%
              select(RAW.PAY.CODE, INCLUDE.HOURS, INCLUDE.EXPENSES,
                     WORKED.PAY.CODE),
            by = c("Pay.Code" = "RAW.PAY.CODE")) %>%
  mutate(INCLUDE.HOURS = as.integer(INCLUDE.HOURS),
         INCLUDE.EXPENSES = as.integer(INCLUDE.EXPENSES),
         WORKED.PAY.CODE = as.integer(WORKED.PAY.CODE))
if (nrow(accrual_raw_detail) != row_count) {
  showDialog(title = "Join error",
             message = paste("Row count failed at", "accrual_raw_detail"))
  stop(paste("Row count failed at", "accrual_raw_detail"))
}

row_count <- nrow(accrual_raw_detail)
accrual_raw_summary <- accrual_raw_detail %>%
  mutate(Hours_Worked = Hours * INCLUDE.HOURS * WORKED.PAY.CODE,
         Expense_Worked = Expense * INCLUDE.EXPENSES * WORKED.PAY.CODE,
         Hours_Paid = Hours * INCLUDE.HOURS,
         Expense_Paid = Expense * INCLUDE.EXPENSES) %>%
  group_by(Facility.Hospital.Id_Worked, DPT.WRKD, Department.Name.Worked.Dept,
           DPT.WRKD.LEGACY, Start.Date, End.Date) %>%
  summarize(Hours_Worked = sum(Hours_Worked, na.rm = TRUE),
            Expense_Worked = sum(Expense_Worked, na.rm = TRUE),
            Hours_Paid = sum(Hours_Paid, na.rm = TRUE),
            Expense_Paid = sum(Expense_Paid, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(map_uni_reports %>%
              select(ORACLE.COST.CENTER, DEFINITION.CODE, DEFINITION.NAME),
            by = c("DPT.WRKD" = "ORACLE.COST.CENTER"))
if (nrow(accrual_raw_detail) != row_count) {
  showDialog(title = "Join error",
             message = paste("Row count failed at", "accrual_raw_summary"))
  stop(paste("Row count failed at", "accrual_raw_summary"))
}

View(accrual_raw_summary)


## Employee Excessive Hours -----------------------------------------------

### Regular Hours ---------------------------------------------------------

employee_reg_hours_qc <- bislr_payroll %>%
  left_join(map_uni_paycodes %>%
              select(RAW.PAY.CODE, INCLUDE.HOURS, WORKED.PAY.CODE,
                     PAY.CODE.CATEGORY) %>%
              distinct(),
            by = c("Pay.Code" = "RAW.PAY.CODE")) %>%
  left_join(date_filtering) %>%
  filter(PROVIDER == 0,
         PAY.CODE.CATEGORY == "REGULAR",
         INCLUDE.HOURS == "1") %>%
  filter(!is.na(upload_date)) %>%
  group_by(Employee.ID, Employee.Name, Start.Date, End.Date) %>%
  summarize(hours_reg_pp = sum(Hours, na.rm = T)) %>%
  ungroup() %>%
  mutate(pp_days = as.numeric(End.Date - Start.Date) + 1) %>%
  mutate(pp_thresh = case_when(
    pp_days == 7 ~ 40,
    pp_days == 14 ~ 80)) %>%
  mutate(pp_overage_coeff = hours_reg_pp / pp_thresh) %>%
  filter(hours_reg_pp > pp_thresh) %>%
  arrange(-pp_overage_coeff, Employee.Name, End.Date, Start.Date)

View(employee_reg_hours_qc)

# create detailed view of all entries for employees with high regular hours
employee_reg_hours_qc_detail <- bislr_payroll %>%
  left_join(map_uni_paycodes %>%
              select(RAW.PAY.CODE, INCLUDE.HOURS, WORKED.PAY.CODE,
                     PAY.CODE.CATEGORY) %>%
              distinct(),
            by = c("Pay.Code" = "RAW.PAY.CODE")) %>%
  left_join(filter(select(map_uni_reports, DEFINITION.CODE, DEFINITION.NAME,
                          ORACLE.COST.CENTER, DEPARTMENT.BREAKDOWN, CLOSED),
                   is.na(CLOSED),
                   DEPARTMENT.BREAKDOWN == 1),
                   c("DPT.WRKD" = "ORACLE.COST.CENTER")) %>%
  inner_join(employee_reg_hours_qc) %>%
  arrange(-pp_overage_coeff, Employee.Name, End.Date, Start.Date,
          DPT.WRKD, Pay.Code)

View(employee_reg_hours_qc_detail)

### Total Hours ----------------------------------------------------------

employee_tot_hours_qc <- bislr_payroll %>%
  left_join(date_filtering) %>%
  left_join(map_uni_paycodes %>%
              select(RAW.PAY.CODE, INCLUDE.HOURS, WORKED.PAY.CODE,
                     PAY.CODE.CATEGORY) %>%
              distinct(),
            by = c("Pay.Code" = "RAW.PAY.CODE")) %>%
  filter(PROVIDER == 0,
         WORKED.PAY.CODE == 1,
         INCLUDE.HOURS == "1") %>%
  filter(!is.na(upload_date)) %>%
  group_by(Employee.ID, Employee.Name, Start.Date, End.Date) %>%
  summarize(hours_tot_pp = sum(Hours, na.rm = T)) %>%
  ungroup() %>%
  mutate(pp_days = as.numeric(End.Date - Start.Date) + 1) %>%
  mutate(pp_thresh = case_when(
    pp_days == 7 ~ 55,
    pp_days == 14 ~ 110)) %>%
  mutate(pp_overage_coeff = hours_tot_pp / pp_thresh) %>%
  filter(hours_tot_pp > pp_thresh) %>%
  arrange(-pp_overage_coeff, Employee.Name, End.Date, Start.Date)

View(employee_tot_hours_qc)

# create detailed view of all entries for employees with high total hours
employee_tot_hours_qc_detail <- bislr_payroll %>%
  left_join(map_uni_paycodes %>%
              select(RAW.PAY.CODE, INCLUDE.HOURS, WORKED.PAY.CODE,
                     PAY.CODE.CATEGORY) %>%
              distinct(),
            by = c("Pay.Code" = "RAW.PAY.CODE")) %>%
  left_join(filter(select(map_uni_reports, DEFINITION.CODE, DEFINITION.NAME,
                          ORACLE.COST.CENTER, DEPARTMENT.BREAKDOWN, CLOSED),
                   is.na(CLOSED),
                   DEPARTMENT.BREAKDOWN == 1),
            c("DPT.WRKD" = "ORACLE.COST.CENTER")) %>%
  inner_join(employee_tot_hours_qc) %>%
  arrange(-pp_overage_coeff, Employee.Name, End.Date, Start.Date,
          DPT.WRKD, Pay.Code)

View(employee_tot_hours_qc_detail)

### Table with Percent of Employees in Excess ------------------------------

# Get total employees by home facility
# Identify percent of employees that are on the
# reg hours qc and total hours qc lists

### Premier reports with excess --------------------------------------------

employee_hr_qc_rpt_potential <- employee_tot_hours_qc_detail %>%
  filter(DEPARTMENT.BREAKDOWN == 1) %>%
  filter(INCLUDE.HOURS == 1,
         WORKED.PAY.CODE == 1) %>%
  group_by(DEFINITION.CODE, DEFINITION.NAME, Start.Date, End.Date) %>%
  summarize(Hours = sum(Hours, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pp_days = as.numeric(End.Date - Start.Date) + 1) %>%
  mutate(FTEs_under_review = case_when(pp_days == 7 ~ round(Hours / 37.5, 2),
                                       pp_days == 14 ~ round(Hours / 75, 2))
  ) %>%
  select(-Hours) %>%
  arrange(-FTEs_under_review)

View(employee_hr_qc_rpt_potential)

# Exporting Data ----------------------------------------------------------

date_range <- paste0(
  format(dist_prev + lubridate::days(1), "%Y-%m-%d"),
  "_to_",
  format(distribution_date, "%Y-%m-%d")
)

## Premier 2.0 output format updates --------------------------------------

### department dict & map -------------------------------------------------

upload_dict_dpt_cols <- c("Corporation Code",
                          "Entity Code",
                          "Cost Center Code",
                          "Cost Center Name")

colnames(upload_dict_dpt) <- upload_dict_dpt_cols

upload_map_dpt <- upload_map_dpt %>%
  mutate(expiration_date = NA) %>%
  relocate(expiration_date, .after = effective_date)

upload_map_dpt_cols <- c("Effective Start Date",
                         "Expiration Date",
                         "Corporation Code",
                         "Entity Code",
                         "Cost Center Code",
                         "Premier Standard Dept Code")

colnames(upload_map_dpt) <- upload_map_dpt_cols


### jobcode dict & map ----------------------------------------------------

upload_dict_jc <- upload_dict_dpt_jc %>%
  mutate(Cost.Center = NULL) %>%
  distinct() %>%
  mutate(dahr = NA,
         esd = NA,
         exp_date = NA)

# there could be another step inserted here to remove any jobcodes
# that have already been defined in the JC dictionary or universal file.
# This part of code is based on the legacy system that is looking for
# department-jobcode combinations that were not previously defined

upload_dict_jc_cols <- c("Corporation Code",
                         "Entity Code",
                         "Job Code",
                         "Job Code Name",
                         "Default Agency Hourly Rate",
                         "Effective Start Date",
                         "Expiration Date")
  
colnames(upload_dict_jc) <- upload_dict_jc_cols


upload_map_dpt_jc <- upload_map_dpt_jc %>%
  mutate(exp_date = NA,
         prem_std_dpt = NA) %>% # it's OK for this to be NA instead of an actual
                                # value.  all JC mappings have blank for the
                                # Premier Std Dept
  relocate(exp_date, .after = effective_date) %>%
  relocate(Job.Code, .before = Cost.Center) %>%
  relocate(prem_std_dpt, .after = Cost.Center)

upload_map_dpt_jc_cols <- c("Effective Start Date",
                            "Expiration Date",
                            "Corporation Code",
                            "Entity Code",
                            "Entity Job Code",
                            "Cost Center Code",
                            "Premier Standard Dept Code",
                            "Premier Standard Job Code",
                            "Allocation Percentage")

colnames(upload_map_dpt_jc) <- upload_map_dpt_jc_cols

### paycode dict & map ----------------------------------------------------

if (exists("upload_dict_paycode")) {
  
  upload_dict_paycode_cols <- c("Corporation Code",
                                "Entity Code",
                                "Pay Code",
                                "Pay Code Name")
  
  colnames(upload_dict_paycode) <- upload_dict_paycode_cols
  
  upload_map_paycode <- upload_map_paycode %>%
    mutate(exp_date = NA) %>%
    relocate(exp_date, .after = eff_date)
  
  upload_map_paycode_cols <- c("Effective Start Date",
                               "Expiration Date",
                               "Corporation Code",
                               "Entity Code",
                               "Entity Pay Code",
                               "Premier Standard Pay Code",
                               "Include Hours",
                               "Include Expenses",
                               "Allocation Percentage")
  
  colnames(upload_map_paycode) <- upload_map_paycode_cols
  
}


### payroll ---------------------------------------------------------------

upload_payroll_cols <- c("Corporation Code",
                         "Home Entity Code",
                         "Home Cost Center Code",
                         "Worked Entity Code",
                         "Worked Cost Center Code",
                         "Start Date",
                         "End Date",
                         "Employee Code",
                         "Employee Name",
                         "Approved Hours per Pay Period",
                         "Job Code",
                         "Pay Code",
                         "Hours",
                         "Expense")

colnames(upload_payroll) <- upload_payroll_cols

## Reference Files --------------------------------------------------------



write.table(upload_dict_dpt,
            file = paste0(dir_BISLR, "/BISLR_Department Dictionary_",
                          date_range, ".csv"),
            row.names = F, col.names = T, sep = ",")


# Dept Mapping is not required in 2.0 and creates an error when uploading
# Keeping command available in script in case this comes back online
# in the future.
# write.table(upload_map_dpt,
#             file = paste0(dir_BISLR, "/BISLR_Department Map_",
#                           date_range, ".csv"),
#             row.names = F, col.names = T, sep = ",", na = "")


write.table(upload_dict_jc,
            file = paste0(dir_BISLR, "/BISLR_Job Code Dictionary_",
                          date_range, ".csv"),
            row.names = F, col.names = T, sep = ",", na = "")


write.table(upload_map_dpt_jc,
            file = paste0(dir_BISLR, "/BISLR_Department Job Code Map_",
                          date_range, ".csv"),
            row.names = F, col.names = T, sep = ",", na = "")


if (exists("upload_dict_paycode")) {
  write.table(upload_dict_paycode,
              file = paste0(dir_BISLR, "/BISLR_Pay Code Dictionary_",
                            date_range, ".csv"),
              row.names = F, col.names = T, sep = ",", na = "")

  write.table(upload_map_paycode,
              file = paste0(dir_BISLR, "/BISLR_Pay Code Map_",
                            date_range, ".csv"),
              row.names = F, col.names = T, sep = ",", na = "")

}

if (nrow(filter_dates) > 0) {
  write.xlsx(as.data.frame(pay_cycles_upload_refresh),
             file = paste0(dir_BISLR, "/Reference",
                           "/Pay cycles uploaded_Tracker", ".xlsx"),
             row.names = F)
}

## Payroll Files --------------------------------------------------------------

# MM: is there a better way to cycle than to use sapply?
# or should this simply be muted so the NULL return is not printed in
# the console?
sapply(
  seq_len(length(unique(upload_payroll$`Worked Entity Code`))),
  function(x) {
    write.table(
      filter(upload_payroll,
             `Worked Entity Code` ==
               unique(upload_payroll$`Worked Entity Code`)[x]),
      file = paste0(dir_BISLR, "/",
                    unique(upload_payroll$`Worked Entity Code`)[x],
                    "_Payroll_", date_range, ".csv"),
      row.names = F, col.names = T, sep = ",")
  }
)

## Quality Files --------------------------------------------------------------

# accrual detail
write_rds(accrual_raw_detail,
          file = paste0(dir_BISLR,
                        "/Quality Checks/Accrual Issue Raw Payroll",
                        "/Raw Accrual Original Payroll",
                        date_range, ".rds"))

# backup the previous fte_summary
file.rename(from = paste0(fte_summary_path, "fte_summary.xlsx"),
            to = paste0(fte_summary_path,
                        "fte_summary_BU_",
                        as.character(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"),
                        ".xlsx"))
# fte_summary
write.xlsx2(as.data.frame(fte_summary),
            file = paste0(fte_summary_path, "fte_summary.xlsx"),
            row.names = F,
            sheetName = "fte_summary",
            append = FALSE)

# backup the previous fte_summary_wide
file.rename(from = paste0(fte_summary_path, "fte_summary_wide.xlsx"),
            to = paste0(fte_summary_path,
                        "fte_summary_wide_BU_",
                        as.character(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"),
                        ".xlsx"))

fte_summary_wide_list <- list(
  "new_dept_PD_FTEs" = fte_summary_new_dept_pd,
  "new_dept_WRK_FTEs" = fte_summary_new_dept_wrk,
  "extremes_PD_FTEs" = fte_summary_extreme_change_pd,
  "extremes_WRK_FTEs" = fte_summary_extreme_change_wrk,
  "PAID_FTEs" = fte_summary_wide_paid,
  "WORKED_FTEs" = fte_summary_wide_worked)

openxlsx::write.xlsx(fte_summary_wide_list,
                     file = paste0(fte_summary_path, "fte_summary_wide.xlsx"),
                     firstActiveRow = 2,
                     firstActiveCol = 6,
                     headerStyle =
                       openxlsx::createStyle(halign = "center",
                                             fgFill = "#ADD8E6",
                                             border = "TopBottomLeftRight"),
                     colWidths = "auto",
                     zoom = 70)

# rename previous piv_wide_check
file.rename(from = paste0(dir_BISLR, "/Quality Checks",
                          "/piv_wide_check", ".csv"),
            to = paste0(dir_BISLR, "/Quality Checks",
                        "/piv_wide_check_BU_",
                        as.character(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"),
                        ".csv"))
# write piv_wide_check
write.csv(piv_wide_check,
          file = paste0(dir_BISLR, "/Quality Checks",
                        "/piv_wide_check", ".csv"),
          row.names = FALSE)

emp_excess_hr_qc_list <- list(
  "rpt_impact_excess_hr" = employee_hr_qc_rpt_potential,
  "emp_reg_hr" = employee_reg_hours_qc,
  "emp_reg_hr_det" = employee_reg_hours_qc_detail,
  "emp_tot_hr" = employee_tot_hours_qc,
  "emp_tot_hr_det" = employee_tot_hours_qc_detail)

openxlsx::write.xlsx(emp_excess_hr_qc_list,
                     file = paste0(dir_BISLR, "/Quality Checks",
                                   "/employee_excess_hours_",
                                   as.character(Sys.time(),
                                                format = "%Y-%m-%d"),
                                   ".xlsx"),
                     firstActiveRow = 2,
                     headerStyle =
                       openxlsx::createStyle(halign = "center",
                                             fgFill = "#ADD8E6",
                                             border = "TopBottomLeftRight"),
                     colWidths = "auto",
                     zoom = 70)

# End of Script -----------------------------------------------------------
