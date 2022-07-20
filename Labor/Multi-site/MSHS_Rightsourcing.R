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
#renv::restore()

# Common Packages
library(readxl)
library(dplyr)
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


# Constants ---------------------------------------------------------------

#placeholder for gui site selection
sites <- "MSHS"

# Functions ---------------------------------------------------------------

# create function to read in most recent .csv in a given path
recent_file <- function(path, file_header = F, encoding = "",
                        delimeter = ",", text_cols = NA, desc_order = 1) {
  df <- file.info(list.files(paste0(path),
                             full.names = T,
                             pattern = "*.csv")) %>%
    arrange(desc(mtime))
  df <- read.csv(rownames(df)[desc_order],
                 stringsAsFactors = F,
                 header = file_header,
                 fileEncoding = encoding,
                 sep = delimeter,
                 colClasses = text_cols)
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
                        delimeter = "\t")

raw_data_prev <- recent_file(path = paste0(project_path, "Source Data"),
                        file_header = T,
                        encoding = "UTF-16LE",
                        delimeter = "\t",
                        desc_order = 2)

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
      "Review the col_check dataframe for details",
      "\r\r",
      "To stop running this script, press \"Cancel\" \r",
      "\r",
      "Press \"OK\" to continue running the script."
    ),
    type = "okcancel"
  )
} else {
  winDialog(
    message = paste0(
      "Column Check has passed.\r",
      "There are no new and/or missing columns.\r",
      "Press \"OK\" to continue."
    ),
    type = "ok"
  )
}

if (col_check_stop == "CANCEL") {
  stop("Script is discontinued by your request.")
}

rm(raw_data_prev)

#user needs most recent zero and upload files
if (sites == "MSHS") {
  msbib_zero <- recent_file(path = paste0(project_path, "MSBIB/Zero"),
                            text_cols = rep("character", 14))
  msbib_upload <- recent_file(path = paste0(project_path, "MSBIB/Uploads"),
                              text_cols = rep("character", 14))
  mshq_zero <- recent_file(path = paste0(project_path, "MSHQ/Zero"),
                           text_cols = rep("character", 14))
  mshq_upload <- recent_file(path = paste0(project_path, "MSHQ/Uploads"),
                             text_cols = rep("character", 14))
} else if (sites == "MSBIB") {
  msbib_zero <- recent_file(path = paste0(project_path, "MSBIB/Zero"),
                            text_cols = rep("character", 14))
  msbib_upload <- recent_file(path = paste0(project_path, "MSBIB/Uploads"),
                              text_cols = rep("character", 14))
} else {
  mshq_zero <- recent_file(path = paste0(project_path, "MSHQ/Zero"),
                           text_cols = rep("character", 14))
  mshq_upload <- recent_file(path = paste0(project_path, "MSHQ/Uploads"),
                             text_cols = rep("character", 14))
}


# Data Pre-processing -----------------------------------------------------
# Cleaning raw data and ensuring that all values are accounted for such as
# blanks and NA. As well as excluding data that may not be used or needed. This
# section can be split into multiple ones based on the data pre-processing
# needed.
# One of the first steps could be to perform initial checks to make sure data is
# in the correct format.  This might also be done as soon as the data is
# imported.


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