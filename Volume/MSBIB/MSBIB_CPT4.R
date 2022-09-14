# Libraries ---------------------------------------------------------------

# library(tidyverse)
library(dplyr)
library(data.table)
library(xlsx)
library(readxl)
library(tidyr)
library(lubridate)
# does sequence of loading affect which functions are masked?

# Assigning Directory(ies) ------------------------------------------------

if ("Presidents" %in% list.files("J:/")) {
  j_drive <- "J:/Presidents"
} else {
  j_drive <- "J:/deans/Presidents"
}

# Data References ---------------------------------------------------------

map_file_folder <- paste0(j_drive,
                          "/SixSigma/MSHS Productivity/Productivity",
                          "/Volume - Data/MSBI Data/Charge Detail",
                          "/Instruction Files")

## Pay cycles -------------------------------------------------------------
dict_pay_cycles <- read_xlsx(
  paste0(
    j_drive, "/SixSigma/MSHS Productivity/Productivity/Universal Data/",
    "Mapping/MSHS_Pay_Cycle.xlsx"
  )
)

# dates originally come in as POSIXct, so they're being converted to Date
# dict_pay_cycles <- dict_pay_cycles %>%
#   mutate(DATE = format(as.Date(DATE), "%m/%d/%Y"),
#          START.DATE = format(as.Date(START.DATE), "%m/%d/%Y"),
#          END.DATE = format(as.Date(END.DATE), "%m/%d/%Y"))

# path_dict_pay_cycles <- choose.files(default = map_file_folder,
#                                            caption = "Select Pay Cycles File",
#                                            multi = F)
# dictionary_pay_cycles <- read_xlsx(path_dict_pay_cycles,
#                                    sheet = 1,
#                                    col_types = c("date", "skip", "skip", "skip",
#                                                  "skip", "skip", "skip", "skip",
#                                                  "skip", "skip", "date", "date",
#                                                  "skip"))
# dictionary_pay_cycles$Date <- as.Date(dictionary_pay_cycles$Date)
# dictionary_pay_cycles$`Start Date` <- as.Date(
#   dictionary_pay_cycles$`Start Date`)
# dictionary_pay_cycles$`End Date` <- as.Date(dictionary_pay_cycles$`End Date`)


## CDM ---------------------------------------------------------------------

CDM_file_path <- choose.files(
  default = paste0(j_drive, "/SixSigma/MSHS Productivity/Productivity/",
                   "Volume - Data/CDMs/BIB/*"),
  caption = "Select CDM File",
  multi = F)
CDM <- read_xlsx(CDM_file_path, sheet = 1)
CDM_slim <- CDM %>%
  select(CHARGE_CODE, CHARGE_DESC, OPTB_cpt4) %>%
  mutate(CHARGE_CODE = stringr::str_trim(CHARGE_CODE),
         OPTB_cpt4 = case_when(
           is.na(OPTB_cpt4) ~ "0",
           TRUE ~ OPTB_cpt4
         )
  )


## crosswalk --------------------------------------------------------------

cc_xwalk_file_path <- choose.files(
  default = paste0(j_drive, "/SixSigma/MSHS Productivity/Productivity/",
                   "Volume - Data/MSBI Data/Charge Detail/Instruction Files/",
                   "*"),
  caption = "Select Crosswalk File", multi = F)
cc_xwalk <- read_xlsx(cc_xwalk_file_path, sheet = 1, 
                      col_types = c("guess", "text", "text"))
# cc_xwalk <- cc_xwalk %>%
#   mutate(`EPSI Revenue Department` = as.character(`EPSI Revenue Department`))
# cc_xwalk$`EPSI Revenue Department` <- as.character(
#   cc_xwalk$`EPSI Revenue Department`)


## CPT Count/RVU map ------------------------------------------------------

# ask Greg L to move this file to Universal?

cpt_ref_path <- choose.files(
  default = paste0(j_drive, "/SixSigma/MSHS Productivity/Productivity/",
                   "Volume - Data/MSH Data/Charges/CPT Reference/",
                   "CPT_ref.xlsx"),
  caption = "Select CPT Reference File", multi = F)
cpt_ref <- read_xlsx(
  cpt_ref_path,
  sheet = 1,
  col_types = c(rep("text", 4), rep("numeric", 10), rep("text", 4))
)


# Constants ---------------------------------------------------------------

# select distribution dates that are less than today, take the max

# Table of distribution dates
dist_dates <- dict_pay_cycles %>%
  select(END.DATE, PREMIER.DISTRIBUTION) %>%
  distinct() %>%
  drop_na() %>%
  arrange(END.DATE) %>%
  #filter only on distribution end dates
  filter(PREMIER.DISTRIBUTION %in% c(TRUE, 1),
         as.POSIXct(END.DATE) < as.POSIXct(Sys.Date()))

#Selecting current distribution date
dist_date <- dplyr::last(dist_dates$END.DATE)
  
#Confirming distribution date which will be the max of the current upload
answer <- winDialog(
  message = paste0(
    "Current distribution will be ", dist_date, "\r\r",
    "Is this correct?\r\r",
    "(If this is not correct,\r",
    "you will be prompted to select the correct\r",
    "distribution date.)"
  ),
  type = "yesno"
)

if (answer == "NO") {
  dist_date <- select.list(
    choices =
      format(sort.POSIXlt(dist_dates$END.DATE, decreasing = T), "%m/%d/%Y"),
    preselect = format(
      sort.POSIXlt(dist_dates$END.DATE, decreasing = T), "%m/%d/%Y")[2],
    multiple = F,
    title = "Select current distribution",
    graphics = T
  )
  dist_date <- mdy(dist_date)
}

# the start date of data will be the date of previous distribution + 1
prev_dist <- dist_dates$END.DATE[which(dist_dates$END.DATE == dist_date) - 1]
date_start <- prev_dist + days(1)


# Data Import -------------------------------------------------------------

# select where the monthly data is pulled from
# this is typically on a hard-drive to expedite the processing time
# but default is set to the shared drive location
work_path <- choose.dir(
  default = paste0(j_drive, "/SixSigma/MSHS Productivity/Productivity",
                   "/Volume - Data/MSBI Data/Charge Detail"),
  caption = "Select folder above all data files")

# get file paths
all_folders <-
  list.dirs(
    path = work_path,
    full.names = TRUE
  )

all_paths <-
  list.files(
    path = all_folders,
    pattern = "*.txt",
    full.names = TRUE
  )

# just file names
all_filenames <- all_paths %>%
  basename() %>%
  as.data.frame()
colnames(all_filenames) <- c("file")

file_ok <- winDialog(message = paste0(
  "The files listed below will be processed.\r\r",
  "Are these correct?\r\r",
  paste(sort(unique(all_filenames$file)),
        collapse = "\n")),
  type = "yesno")

if (file_ok == "NO") {
  stop("Script is stopped so files can be organized for proper import.")
}

# read file content
all_content <-
  all_paths %>%
  lapply(read.table,
         header = TRUE,
         sep = "|",
         encoding = "UTF-8",
         stringsAsFactors = F
  )


# combine file content list and file name list
all_filenames <- all_paths %>%
  basename() %>%
  as.list()

# append file name to each data point
all_lists <- mapply(c, all_content, all_filenames, SIMPLIFY = FALSE)

# unlist all lists and change column name
all_result <- rbindlist(all_lists, fill = T)
# change column name
names(all_result)[10] <- "file_path"


## confirm date range of data ----------------------------------------------



## save merged files for reference -----------------------------------------

saveRDS(all_result, file = paste0(work_path,
                                  "/all_merged_",
                                  date_start,
                                  "_to_",
                                  dist_date,
                                  ".rds"))
write.table(all_result, file = paste0(month_dir, "/all_merged.csv"),
            row.names = F, col.names = T, sep = ",")


## import rds if preferred ------------------------------------------------

# all_result <- readRDS(file = paste0(month_dir, "/all_merged.rds"))

# could make this an if statement earlier in the process
# and prompt user about data import process


## remove unneeded data ---------------------------------------------------

rm(all_lists, all_content)


# Data Pre-processing -----------------------------------------------------

all_result$TransDate <- as.Date(all_result$TransDate, format = "%m/%d/%Y")
all_result$ChargeCode <- stringr::str_trim(all_result$ChargeCode)

# all_result <- all_result %>%
#   filter(EntityId == "MSBI" | EntityId == "MSCCW")

all_result <- left_join(x = all_result,
                        y = dictionary_pay_cycles,
                        by = c("TransDate" = "Date"),
                        all.x = T)

all_result <- left_join(x = all_result,
                        y = CC_xwalk,
                        by = c("FacilityId" = "FacilityId",
                               "RevDept" = "EPSI Revenue Department"),
                        all.x = T)

all_result <- left_join(x = all_result,
                        y = select(CDM_slim, -any_of("CHARGE_DESC")),
                        by = c("ChargeCode" = "CHARGE_CODE"),
                        all.x = T)

all_result$OPTB_cpt4[is.na(all_result$OPTB_cpt4)] <- "#N/A"

NA_CC_result <- all_result %>%
  filter(is.na(`Labor Department`) |
           str_detect(`Labor Department`, "NOMAP")) %>%
  group_by(FacilityId, RevDept) %>%
  summarise(Vol = sum(Qty))

NA_CPT4_result <- all_result %>%
  filter(OPTB_cpt4 == "#N/A" | OPTB_cpt4 == 0 |
           OPTB_cpt4 == 99999 | is.na(OPTB_cpt4)) %>%
  group_by(FacilityId, RevDept, ChargeCode, OPTB_cpt4) %>%
  summarise(Vol = sum(Qty))

charge_summary <- all_result %>%
  group_by(`Labor Department`, TransDate, OPTB_cpt4) %>%
  summarise(Vol = sum(Qty))

charge_summary <- charge_summary %>%
  filter(TransDate >= begin_date &
           TransDate <= final_date)

# Data Formatting ---------------------------------------------------------

charge_summary$EntityID <- rep(729805, nrow(charge_summary))
charge_summary$FacilID <- rep(630571, nrow(charge_summary))
charge_summary$budget <- rep(0, nrow(charge_summary))
charge_summary <- charge_summary %>%
  mutate(EndDate = TransDate) %>%
  rename(StartDate = TransDate) %>%
  select(EntityID, FacilID, `Labor Department`, StartDate, EndDate,
         OPTB_cpt4, Vol, budget) %>%
  filter(!is.na(`Labor Department`)) %>%
  filter(!is.na(OPTB_cpt4)) %>%
  filter(OPTB_cpt4 != "#N/A") %>%
  filter(OPTB_cpt4 != 0) %>%
  filter(OPTB_cpt4 != 99999) %>%
  filter(!str_detect(`Labor Department`, "NOMAP"))

charge_summary$StartDate <- format(charge_summary$StartDate, "%m/%d/%Y")
charge_summary$EndDate <- format(charge_summary$EndDate, "%m/%d/%Y")

# Quality Checks ----------------------------------------------------------


# File Saving -------------------------------------------------------------

write.table(charge_summary, file = paste0(month_dir, "/CPT upload.csv"),
            row.names = F, col.names = F, sep = ",")
write.table(NA_CC_result, file = paste0(month_dir, "/CPT no CC.csv"),
            row.names = F, col.names = T, sep = ",")
write.table(NA_CPT4_result, file = paste0(month_dir, "/CPT no CPT4 map.csv"),
            row.names = F, col.names = T, sep = ",")

# Script End --------------------------------------------------------------
