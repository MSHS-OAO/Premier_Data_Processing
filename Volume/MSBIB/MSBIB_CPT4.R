# Libraries ---------------------------------------------------------------

library(dplyr)
library(data.table)
library(xlsx)
library(readxl)
library(tidyr)
library(lubridate)
library(stringr)
library(rstudioapi)
library(DBI)
library(odbc)

# does sequence of loading affect which functions are masked?

# Assigning Directory(ies) ------------------------------------------------

j_drive <- "/SharedDrive/deans/Presidents"

oao_con <- dbConnect(odbc(), "OAO Cloud DB Production")


# Data References ---------------------------------------------------------

map_file_folder <- paste0(j_drive,
                          "/SixSigma/MSHS Productivity/Productivity",
                          "/Volume - Data/MSBI Data/Charge Detail",
                          "/Instruction Files")

## Pay cycles -------------------------------------------------------------
dict_pay_cycles <- tbl(oao_con, "LPM_MAPPING_PAYCYCLE") %>% collect()
# modifying column names in order to not have to recode the rest of the script
dict_pay_cycles <- dict_pay_cycles %>%
  rename(DATE = PAYCYCLE_DATE,
         START.DATE = PP_START_DATE,
         END.DATE = PP_END_DATE,
         PREMIER.DISTRIBUTION = PREMIER_DISTRIBUTION)


## CDM ---------------------------------------------------------------------

cdm_folder_info <- file.info(
  list.files(paste0(j_drive, "/SixSigma/MSHS Productivity/Productivity/",
                    "Volume - Data/cdms/BIB/"),
             full.names = TRUE,
             pattern = "*.xlsx")) %>%
  arrange(desc(mtime))

cdm_file_path <- rownames(cdm_folder_info)[1]
cdm <- read_xlsx(cdm_file_path, sheet = 1)
cdm_slim <- cdm %>%
  select(CHARGE_CODE, CHARGE_DESC, OPTB_cpt4) %>%
  mutate(CHARGE_CODE = str_trim(CHARGE_CODE),
         OPTB_cpt4 = case_when(
           is.na(OPTB_cpt4) ~ "0",
           TRUE ~ OPTB_cpt4
         )
  )

rm(cdm)

## crosswalk --------------------------------------------------------------

cc_xwalk_file_path <- paste0(j_drive,
                             "/SixSigma/MSHS Productivity/Productivity/",
                             "Volume - Data/MSBI Data/Charge Detail/",
                             "Instruction Files/",
                             "MSBIB Charge Detail Rev to Labor Dept Crosswalk",
                             ".xlsx")
cc_xwalk <- read_xlsx(cc_xwalk_file_path, sheet = 1,
                      col_types = c("guess", rep("text", 8), "skip"))


## CPT Count/RVU map ------------------------------------------------------

# ask Greg L to move this file to Universal?

cpt_ref_path <- paste0(j_drive, "/SixSigma/MSHS Productivity/Productivity/",
                   "Volume - Data/MSH Data/Charges/CPT Reference/",
                   "CPT_ref.xlsx")
cpt_ref <- read_xlsx(
  cpt_ref_path,
  sheet = 1,
  col_types = c(rep("text", 4), rep("numeric", 10), rep("text", 4))
)

cpt_ref_slim <- cpt_ref %>%
  select(`Effective Year/Quarter`, `CPT/HCPCS Code`, `Modifier Code`,
         `Facility Practice Expense RVU Factor`, `CPT Procedure Count`,
         `Short Description`, `Long Description`) %>%
  filter(`Effective Year/Quarter` ==
           unique(cpt_ref$`Effective Year/Quarter`)[1])

rm(cpt_ref)


# Constants ---------------------------------------------------------------

# select distribution dates that are less than today, take the max

# Table of distribution dates
dist_dates <- dict_pay_cycles %>%
  select(END.DATE, PREMIER.DISTRIBUTION) %>%
  distinct() %>%
  drop_na() %>%
  arrange(END.DATE) %>%
  #filter only on distribution end dates
  filter(PREMIER.DISTRIBUTION %in% c(TRUE, 1, "1"),
         as.POSIXct(END.DATE) < as.POSIXct(Sys.Date()))

# Selecting current distribution date
dist_date <- dplyr::last(dist_dates$END.DATE)

# Confirming distribution date which will be the max of the current upload
answer <- showQuestion(
  title = "Distribution Date Confirmation",
  message = paste0(
    "Current distribution will be ", dist_date, ".  ",
    "If this is correct, press OK.  ",
    "If this is not correct, press Cancel and ",
    "you will be prompted to select the correct ",
    "distribution date."
  )
)
if (answer == FALSE) {
  dist_date <- select.list(
    choices =
      format(sort.POSIXlt(dist_dates$END.DATE, decreasing = TRUE), "%m/%d/%Y"),
    preselect = format(
      sort.POSIXlt(dist_dates$END.DATE, decreasing = TRUE), "%m/%d/%Y")[2],
    multiple = FALSE,
    title = "Select current distribution",
    graphics = TRUE
  )
  dist_date <- mdy(dist_date)
}

# the start date of data will be the date of previous distribution + 1
prev_dist <- dist_dates$END.DATE[which(dist_dates$END.DATE == dist_date) - 1]
date_start <- prev_dist + days(1)


# Data Import -------------------------------------------------------------

raw_path <- paste0(j_drive,
                     "/SixSigma/MSHS Productivity/Productivity/",
                     "Volume - Data/MSBI Data/Charge Detail/Source Data")

# get file paths
all_folders <-
  list.dirs(
    path = raw_path,
    full.names = TRUE
  )

all_paths <-
  list.files(
    path = all_folders,
    pattern = "*.txt",
    full.names = TRUE
  )

# dates for file naming convention from Raymund
file_date_names <- dict_pay_cycles %>%
  select(-DATE) %>%
  distinct() %>%
  filter(START.DATE >= date_start,
         END.DATE <= dist_date + days(14)) %>%
  mutate(range_desc = paste0("F",
                             gsub(" ", "",
                                  gsub(" 0", " ",
                                       format(START.DATE, " %m %d %Y")
                                  )),
                             "T",
                             gsub(" ", "",
                                  gsub(" 0", " ",
                                       format(END.DATE, " %m %d %Y")))))

all_paths_incl <- as.data.frame(all_paths) %>%
  rename(path = all_paths) %>%
  filter(str_detect(path,
                    paste(file_date_names$range_desc, collapse = "|")))

use_paths <- all_paths_incl[["path"]]

# just file names
all_filenames <- use_paths %>%
  basename() %>%
  as.data.frame()
colnames(all_filenames) <- c("file")

file_ok <- showQuestion(
  title = "Files to use",
  message = paste0(
  "The files listed below will be processed.  ",
  "Are these correct?  ",
  paste(sort(unique(all_filenames$file)),
        collapse = "\n"))
  )

if (file_ok == FALSE) {
  stop("Script is stopped so files can be organized for proper import.")
}

# read file content
all_content <-
  use_paths %>%
  lapply(read.table,
         header = TRUE,
         sep = "|",
         encoding = "UTF-8",
         stringsAsFactors = FALSE
  )


# combine file content list and file name list
all_filenames <- use_paths %>%
  basename() %>%
  as.list()

# append file name to each data point
all_lists <- mapply(c, all_content, all_filenames, SIMPLIFY = FALSE)

# unlist all lists and change column name
raw_data <- rbindlist(all_lists, fill = TRUE)
# change column name
raw_data <- raw_data %>%
  rename(file_path = V1)

## save merged files for reference -----------------------------------------

write_path_root <- paste0(j_drive, "/SixSigma/MSHS Productivity/Productivity",
                          "/Volume - Data/MSBI Data/Charge Detail",
                          "/Calculation Worksheets")

write_path <- paste0(write_path_root,
                     "/",
                     format(dist_date, "%Y-%m-%d"),
                     " MSBIB")

dir.create(write_path)

saveRDS(raw_data,
        file = paste0(write_path, "/all_merged_", date_start, "_to_", dist_date,
                                  ".rds"))
write.table(raw_data,
            file = paste0(write_path, "/all_merged_", date_start, "_to_",
                          dist_date, ".csv"),
            row.names = FALSE, col.names = TRUE, sep = ",")


## alternative to import raw data rds --------------------------------------

# this is rarely done so user can uncomment and run function
# on their own if desired

# raw_data <- readRDS(file = choose.files()))


## remove unneeded data ---------------------------------------------------

rm(all_content, all_lists, all_folders, all_paths, all_filenames)


# Data Pre-processing -----------------------------------------------------

processed_data <- raw_data

processed_data <- processed_data %>%
  mutate(TransDate = as.Date(TransDate, format = "%m/%d/%Y"),
         ChargeCode = str_trim(ChargeCode),
         `Modifier Code` = as.character(NA))

# join pay cycle info, cc_xwalk, cdm, and Premier CPT counter
processed_data <- processed_data %>%
  left_join(y = dict_pay_cycles,
            by = c("TransDate" = "DATE")) %>%
  left_join(y = cc_xwalk,
            by = c("FacilityId" = "FacilityId",
                   "RevDept" = "EPSI Revenue Department")) %>%
  left_join(y = select(cdm_slim, -any_of("CHARGE_DESC")),
            by = c("ChargeCode" = "CHARGE_CODE")) %>%
  mutate(OPTB_cpt4 = case_when(
    is.na(OPTB_cpt4) ~ "#N/A",
    TRUE ~ OPTB_cpt4)) %>%
  left_join(y = cpt_ref_slim,
            by = c("OPTB_cpt4" = "CPT/HCPCS Code",
                   "Modifier Code" = "Modifier Code"))

charge_summary <- processed_data %>%
  group_by(`Labor Department`, TransDate, OPTB_cpt4, `Published Report`) %>%
  summarise(Vol = sum(Qty)) %>%
  ungroup() %>%
  filter(TransDate >= date_start &
           TransDate <= dist_date) %>%
  filter(!is.na(`Labor Department`)) %>%
  filter(!is.na(OPTB_cpt4)) %>%
  filter(OPTB_cpt4 != "#N/A") %>%
  filter(OPTB_cpt4 != "0") %>%
  filter(OPTB_cpt4 != "99999") %>%
  filter(!str_detect(`Labor Department`, "NOMAP"))

# Data Formatting ---------------------------------------------------------

upload <- charge_summary %>%
  filter(`Published Report` == "yes") %>%
  mutate(EntityID = 729805,
         FacilID = 630571,
         budget = 0,
         EndDate = TransDate) %>%
  rename(StartDate = TransDate) %>%
  mutate(StartDate = format(StartDate, "%m/%d/%Y"),
         EndDate = format(EndDate, "%m/%d/%Y")) %>%
  select(EntityID, FacilID, `Labor Department`, StartDate, EndDate,
         OPTB_cpt4, Vol, budget)

non_upload_depts <- charge_summary %>%
  filter(is.na(`Published Report`) | `Published Report` != "yes") %>%
  mutate(EntityID = 729805,
         FacilID = 630571,
         budget = 0,
         EndDate = TransDate) %>%
  rename(StartDate = TransDate) %>%
  mutate(StartDate = format(StartDate, "%m/%d/%Y"),
         EndDate = format(EndDate, "%m/%d/%Y")) %>%
  select(EntityID, FacilID, `Labor Department`, StartDate, EndDate,
         OPTB_cpt4, Vol, budget)


## 0 vol for missing days ----------------------------------------------------

date_range <- data.frame(
  StartDate = seq(from = as.Date(date_start), to = as.Date(dist_date),
                  by = "day"),
  EndDate = seq(from = as.Date(date_start), to = as.Date(dist_date),
                by = "day"))

cc_xwalk_unique <- cc_xwalk %>%
  filter(`Published Report` == "yes") %>%
  select(`Labor Department`) %>%
  unique()

xwalk_and_date <- merge(date_range, cc_xwalk_unique) %>%
  mutate(StartDate = as.character(StartDate, "%m/%d/%Y"),
         EndDate = as.character(EndDate, "%m/%d/%Y")) %>%
  mutate(EntityID = 729805,
         FacilID = 630571,
         budget = 0)

upload <- upload %>%
  right_join(xwalk_and_date) %>%
  mutate(Vol = replace_na(Vol, 0),
         OPTB_cpt4 = replace_na(OPTB_cpt4, "G0463"))

## Premier 2.0 Headers ------------------------------------------------------

upload_cols <- c("Corporation Code",
                 "Entity Code",
                 "Cost Center Code",
                 "Start Date",
                 "End Date",
                 "CPT Code",
                 "Actual Volume",
                 "Budget Volume")

colnames(upload) <- upload_cols

# Quality Checks ----------------------------------------------------------


## volumes in Premier published reports -----------------------------------
charge_summary_qc <- processed_data %>%
  filter(`Published Report` == "yes") %>%
  filter(END.DATE >= date_start &
           START.DATE <= dist_date) %>%
  mutate(prem_vol = case_when(
    `Report Metric Type` == "RVU" ~
      Qty * `Facility Practice Expense RVU Factor`,
    `Report Metric Type` == "CPT" ~ Qty * `CPT Procedure Count`)) %>%
  group_by(`Report ID`, `Report Name`, `Report Metric`, `Report Metric Type`,
           START.DATE, END.DATE, ) %>%
  summarize(Vol = sum(prem_vol)) %>%
  ungroup()

View(charge_summary_qc)

## rev dept not mapped ----------------------------------------------------

na_cc_summary <- processed_data %>%
  filter(is.na(`Labor Department`) |
           str_detect(`Labor Department`, "NOMAP")) %>%
  filter(END.DATE > date_start & START.DATE < dist_date) %>%
  group_by(FacilityId, RevDept, `Labor Department`, START.DATE, END.DATE) %>%
  summarise(vol = sum(Qty),
            prem_cpt = sum(Qty * `CPT Procedure Count`, na.rm = TRUE)) %>%
  ungroup()

View(na_cc_summary)

## charge not mapped to a CPT --------------------------------------------

na_cpt4_summary <- processed_data %>%
  filter(OPTB_cpt4 == "#N/A" | OPTB_cpt4 == 0 |
           OPTB_cpt4 == 99999 | is.na(OPTB_cpt4)) %>%
  filter(END.DATE > date_start & START.DATE < dist_date) %>%
  group_by(ChargeCode, OPTB_cpt4) %>%
  summarise(vol = sum(Qty)) %>%
  ungroup()

View(na_cpt4_summary)

## charge not mapped to CPT for published report ---------------------------

na_cpt4_pub_report_summary <- processed_data %>%
  filter(`Published Report` == "yes") %>%
  filter(OPTB_cpt4 == "#N/A" | OPTB_cpt4 == 0 |
           OPTB_cpt4 == 99999 | is.na(OPTB_cpt4)) %>%
  filter(END.DATE > date_start & START.DATE < dist_date) %>%
  group_by(`Report ID`, `Report Name`, `Report Metric`, `Report Metric Type`,
           START.DATE, END.DATE, ChargeCode, OPTB_cpt4) %>%
  summarise(vol = sum(Qty)) %>%
  ungroup()

View(na_cpt4_pub_report_summary)

# File Saving -------------------------------------------------------------

date_start_char <- format(as.Date(date_start), "%Y-%m-%d")
date_dist_char <- format(as.Date(dist_date), "%Y-%m-%d")

write.table(upload,
            file = paste0(write_path, "/MSBIB CPT Vol ", date_start_char,
                          " to ", date_dist_char, ".csv"),
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(non_upload_depts,
            file = paste0(write_path, "/MSBIB CPT Vol - depts not pub ",
                          date_start_char, " to ", date_dist_char, ".csv"),
            row.names = FALSE, col.names = TRUE, sep = ",")

write.table(na_cc_summary,
            file = paste0(write_path, "/CPT no CC ", date_start_char,
                          " to ", date_dist_char, ".csv"),
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(na_cpt4_summary,
            file = paste0(write_path, "/Charge no CPT4 map ", date_start_char,
                          " to ", date_dist_char, ".csv"),
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(na_cpt4_pub_report_summary,
            file = paste0(write_path, "/Charge no CPT4 map Prem Pub ",
                          date_start_char, " to ", date_dist_char, ".csv"),
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(charge_summary_qc,
            file = paste0(write_path, "/charge reports prem vol ",
                          date_start_char, " to ", date_dist_char, ".csv"),
            row.names = FALSE, col.names = TRUE, sep = ",")

# Script End --------------------------------------------------------------
