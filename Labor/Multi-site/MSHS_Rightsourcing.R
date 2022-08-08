# GENERAL REMINDERS -------------------------------------------------------

# 1. The outline can be customized based on project requirements.
# 2. Sections may be added, removed, reordered, renamed, etc. as needed.
# 3. Reference checklist for commenting and style guidelines
# 4. Create Sections using the menu Code > Insert Section
#    or the shortcut Ctrl+Shift+R
# 5. Create subsections by starting a Section with two # signs (e.g. ##).
#    - See example within this section.

# Libraries ---------------------------------------------------------------
# Include all the packages that will be used throughout the code.
# This is where packages can be installed if the user does not have them
# currently installed.

# Ensuring the appropriate package versions are used for the project based on
# renv usage
# renv::restore()

# Common Packages
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
# library(rmarkdown)
# library(shiny)

# tidyverse includes: dplyr, ggplot2, lubridate, purrr, readr, readxl,
# reprex, stringr, tidyr...and more
# See this link for full list: https://tidyverse.tidyverse.org/
# library(tidyverse)


# Assigning Directory ------------------------------------------------

# rightsourcing project path
project_path <- paste0("//researchsan02b/shr2/deans/Presidents/SixSigma/",
                       "MSHS Productivity/Productivity/Labor - Data/",
                       "Rightsourcing Labor/")
# universal mapping path
mapping_path <- paste0("//researchsan02b/shr2/deans/Presidents/SixSigma/",
                       "MSHS Productivity/Productivity/universal Data/",
                       "Mapping/")

# Functions ---------------------------------------------------------------

# create function to read in most recent .csv in a given path
recent_file <- function(path, file_header = F, encoding = "",
                        delimeter = ",", text_cols = NA, desc_order = 1,
                        premier = TRUE) {
  df <- file.info(list.files(paste0(path),
                             full.names = T,
                             pattern = "*.csv")) %>%
    arrange(desc(mtime))
  df <- read.csv(rownames(df)[desc_order],
                 stringsAsFactors = F,
                 header = file_header,
                 fileEncoding = encoding,
                 sep = delimeter,
                 colClasses = text_cols,
                 na.strings = c("","NA"))

  # need names on columns of previous month's files
  if (premier == TRUE) {
    prem_upload_col_names <- c("partner",
                             "hosp.home", "dept.home",
                             "hosp.worked", "dept.worked",
                             "date.start", "date.end",
                             "emp.ID", "emp.name",
                             "budget", "jobcode", "paycode",
                             "hours", "spend")

  colnames(df) <- prem_upload_col_names
  }
  
  return(df)
}

# Data Import / Data References --------------------------------------------

# jobcode list to map job description to job code
jobcode_list <- read.csv(paste0(project_path,
                                "Rightsource Job Code.csv"))
# pay period mapping file to determine max date of next upload
pay_period_mapping <- read_xlsx(paste0(mapping_path,
                                       "MSHS_Pay_Cycle.xlsx"))
# code conversion mapping file to convert legacy to oracle cc
code_conversion <- read_xlsx(paste0(mapping_path,
                                    "MSHS_Code_Conversion_Mapping.xlsx"))

# user needs most recent raw data file
raw_data <- recent_file(path = paste0(project_path, "Source Data"),
                        file_header = T,
                        encoding = "UTF-16LE",
                        delimeter = "\t",
                        premier = FALSE)

# user needs previous raw data file to compare column headers
raw_data_prev <- recent_file(path = paste0(project_path, "Source Data"),
                        file_header = T,
                        encoding = "UTF-16LE",
                        delimeter = "\t",
                        desc_order = 2,
                        premier = FALSE)

new_col <-
  colnames(raw_data)[!(colnames(raw_data) %in% colnames(raw_data_prev))]
new_col <- new_col %>%
  data.frame()
colnames(new_col) <- c("Column")
new_col <- new_col %>%
  mutate(Status = "new")

missing_col <-
  colnames(raw_data)[!(colnames(raw_data_prev) %in% colnames(raw_data))]
missing_col <- missing_col %>%
  data.frame()
colnames(missing_col) <- c("Column")
missing_col <- missing_col %>%
  mutate(Status = "missing")

col_check <- rbind(new_col, missing_col)

if (length(col_check$Column) > 0) {
  col_check_stop <- winDialog(
    message = paste0(
      "There are columns that are new and/or missing.\r",
      "Review the col_check dataframe for details\r",
      "\r",
      "To stop running this script, press \"Cancel\" \r",
      "\r",
      "If you have already confirmed that the data is ok\r",
      "press \"OK\" to continue running the script."
    ),
    type = "okcancel"
  )
} else {
  col_check_stop <- "OK"
}

if (col_check_stop == "CANCEL") {
  stop("Script is discontinued by your request.")
}

rm(raw_data_prev)

#user needs most recent zero and upload files
msbib_zero_old <- recent_file(path = paste0(project_path, "MSBIB/Zero"),
                              text_cols = rep("character", 14))
msbib_upload_old <- recent_file(path = paste0(project_path, "MSBIB/Uploads"),
                                text_cols = rep("character", 14))
mshq_zero_old <- recent_file(path = paste0(project_path, "MSHQ/Zero"),
                             text_cols = rep("character", 14))
mshq_upload_old <- recent_file(path = paste0(project_path, "MSHQ/Uploads"),
                               text_cols = rep("character", 14))

# Constants ------------------------------------------------------

# user needs to select the site(s) they want to process rightsourcing for
sites <- select.list(
  choices = c("MSHS", "MSBIB", "MSHQ"),
  title = "Select Output Site(s)",
  graphics = T,
  preselect = "MSHS"
)

#Table of distribution dates
dist_dates <- pay_period_mapping %>%
  select(END.DATE, PREMIER.DISTRIBUTION) %>%
  distinct() %>%
  drop_na() %>%
  arrange(END.DATE) %>%
  #filter only on distribution end dates
  filter(PREMIER.DISTRIBUTION %in% c(TRUE, 1),
         #filter 2 weeks from run date (14 days) for data collection lag
         #before run date
         END.DATE < as.POSIXct(Sys.Date() - 14))

#Selecting current distribution date
distribution_date <- dist_dates$END.DATE[nrow(dist_dates)]

#Confirming distribution date which will be the max of the current upload
answer <- winDialog(
  message = paste0(
    "Current distribution will be ", distribution_date, "\r\r",
    "If this is correct, press OK\r\r",
    "If this is not correct, press Cancel and\r",
    "you will be prompted to select the correct\r",
    "distribution date."
    ),
  type = "okcancel"
)

if (answer == "CANCEL") {
  distribution_date <- select.list(
    choices =
      format(sort.POSIXlt(dist_dates$END.DATE, decreasing = T), "%m/%d/%Y"),
    multiple = F,
    title = "Select current distribution",
    graphics = T
  )
  distribution_date <- mdy(distribution_date)
}

# max date of the previous zero files will be used to determine what the
# min date is of the current upload and zero files
prev_0_max_date_mshq <- max(mdy(mshq_zero_old$date.end))

prev_0_max_date_msbib <- max(mdy(msbib_zero_old$date.end))

# Data Pre-processing -----------------------------------------------------

## New Zero Upload ---------------------------------------------------------

# create zero upload for MSBIB
msbib_zero_new <- msbib_upload_old %>%
  filter(mdy(date.start) > prev_0_max_date_msbib) %>%
  mutate(hours = "0",
         spend = "0")

# create zero upload for MSHQ
mshq_zero_new <- mshq_upload_old %>%
  filter(mdy(date.start) > prev_0_max_date_mshq) %>%
  mutate(hours = "0",
         spend = "0")

## Upload Preprocessing -------------------------------------------------------

# filter raw data on date range needed for upload
processed_data <- raw_data %>%
  filter(mdy(Date.Worked) > min(c(prev_0_max_date_mshq,
                                  prev_0_max_date_msbib)),
         mdy(Date.Worked) <= distribution_date) 

# process department.billed to get oracle home and legacy worked department
processed_data <- processed_data %>%
  filter(Department.Billed != "") %>%
  mutate(cost_center_info = gsub('^.*:\\s*|\\s*\\*.*$', '',
                                 Department.Billed)) %>%
  mutate(wrkd_dept_leg = case_when(
    nchar(cost_center_info) == 12 ~ substr(cost_center_info, 1, 8),
    nchar(cost_center_info) == 30 ~ str_c(substr(cost_center_info, 1, 4),
                                          substr(cost_center_info, 13, 14),
                                          substr(cost_center_info, 16, 19)),
    TRUE ~ cost_center_info)
  ) %>%
  mutate(home_dept_oracle = case_when(
    substr(wrkd_dept_leg, 1, 4) == "0130" ~ "101010101010102",
    substr(wrkd_dept_leg, 1, 4) == "4709" ~ "900000040790000",
    nchar(cost_center_info) == 12 ~ "101010101010101",
    nchar(cost_center_info) == 30 ~ "900000040490000",
    TRUE ~ cost_center_info
  )) %>%
  mutate(hospital = case_when(
    nchar(cost_center_info) == 12 ~ "NY0014",
    nchar(cost_center_info) == 30 ~ "630571",
    TRUE ~ cost_center_info
  ))

# join legacy departments to oracle departments for premier format
row_count <- nrow(processed_data)
processed_data <- processed_data %>%
  left_join(select(code_conversion, COST.CENTER.LEGACY, COST.CENTER.ORACLE), 
            by = c("wrkd_dept_leg" = "COST.CENTER.LEGACY")) %>%
  mutate(wrkd_dept_oracle = case_when(
    is.na(COST.CENTER.ORACLE) ~ home_dept_oracle,
    TRUE ~ COST.CENTER.ORACLE
  ))

# quality check left join to make sure row count has not changed
if (row_count != nrow(processed_data)) {
  winDialog(
    message = paste0("Error in code conversion mapping.",
                     " Row count has been changed by left join"),
    type = "ok"
  )
  stop(paste0("Error in code conversion mapping.",
              " Row count has been changed by left join"))
}

## Job Code Handling -----------------------------------------------------

# process job titles to allow for job code mapping
processed_data <- processed_data %>%
  mutate(Job.Title = replace_na(Job.Title, "Unknown"),
         Job.Title = paste("Rightsourcing", Job.Title))


# new job titles need a Rightsourcing Job Code created
jc_new <- processed_data %>%
  filter(!(Job.Title %in% jobcode_list$Job.Title)) %>%
  select(Job.Title) %>%
  distinct() %>%
  mutate(jobcode = row_number()) %>%
  mutate(jobcode = jobcode + length(jobcode_list$jobcode)) %>%
  mutate(jobcode = stringr::str_pad(jobcode, 5, side = "left", pad = "0")) %>%
  mutate(jobcode = paste0("R", jobcode))

jobcode_list_new <- rbind(jobcode_list, jc_new)

# need QC check for length of jobcode_list_new?
# need an if statement to only create jobcode_list_new if jc_new is not 
#  0 observations or NULL?
# need to skip writing jobcode_list_new if there are no new jobcodes?

# join existing job codes
row_count <- nrow(processed_data)
processed_data <- processed_data %>%
  left_join(jobcode_list) 

# quality check left join to make sure row count has not changed
if (row_count != nrow(processed_data)) {
  winDialog(
    message = paste0("Error in job code mapping.",
                     " Row count has been changed by left join"),
    type = "ok"
  )
  stop(paste0("Error in job code mapping.",
              " Row count has been changed by left join"))
}

jc_dict_upload <- processed_data %>%
  select(hospital, wrkd_dept_oracle, jobcode, Job.Title) %>%
  mutate(system = "729805") %>%
  relocate(system, .before = hospital)
# this may need to be separated into each site's upload file
# this could also be moved to be created later


## Summarizing Hours and Expenses-------------------------------------------

# all types of daily hours need to be summed up
processed_data <- processed_data %>%
  mutate(
    Regular.Hours = case_when(
      is.na(Regular.Hours) ~ 0,
      TRUE ~ Regular.Hours),
    OT.Hours = case_when(
      is.na(OT.Hours) ~ 0,
      TRUE ~ OT.Hours),
    Holiday.Hours = case_when(
      is.na(Holiday.Hours) ~ 0,
      TRUE ~ Holiday.Hours),
    Call.Back.Hours = case_when(
      is.na(Call.Back.Hours) ~ 0,
      TRUE ~ as.numeric(Call.Back.Hours))
  ) %>%
  mutate(worked_hours =
           Regular.Hours + OT.Hours + Holiday.Hours + Call.Back.Hours)
# is there a more efficient way of getting blank values with a 0 instead of NA?

# Day Spend needs to be in numerical decimal format to summarize with it
processed_data <- processed_data %>%
  mutate(Day.Spend.char = Day.Spend,
         Day.Spend = as.numeric(stringr::str_trim(gsub("[$,]", "", Day.Spend))))

# need to summarize data
rolled_up <- processed_data %>%
  group_by(hospital, home_dept_oracle,  wrkd_dept_oracle, Earnings.E.D,
           Worker.Name, jobcode) %>%
  summarize(across(c(worked_hours, Day.Spend), sum, na.rm = T)) %>%
  mutate(week_hours = worked_hours,
         week_spend = Day.Spend)
# needs to be checked for accuracy
# calculation also seems to take time, if we filter down before summarizing
#  (like by date), then this could be made faster


# Data Formatting ---------------------------------------------------------
# How the data will look during the output of the script.
# For example, if you have a data table that needs the numbers to show up as
# green or red depending on whether they meet a certain threshold.


# Quality Checks ----------------------------------------------------------
# Checks that are performed on the output to confirm data consistency and
# expected outputs.


# Visualization -----------------------------------------------------------
# How the data will be plotted or how the data table will look including axis
# titles, scales, and color schemes of graphs or data tables.
# (This section may be combined with the Data Formatting section.)

# For Matt & Greg - perhaps there's a quick plot that can be used as a quality
# check.

# File Saving -------------------------------------------------------------
# Writing files or data for storage


# Script End --------------------------------------------------------------