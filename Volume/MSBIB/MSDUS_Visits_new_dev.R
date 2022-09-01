# GENERAL REMINDERS -------------------------------------------------------

# 1. The outline can be customized based on project requirements.
# 2. Sections may be added, removed, reordered, renamed, etc. as needed.
# 3. Reference checklist for commenting and style guidelines
# 4. Create Sections using the menu Code > Insert Section
#    or the shortcut Ctrl+Shift+R
# 5. Create subsections by starting a Section with two # signs (e.g. ##).  
#    - See example within this section.


## subsection example ----------------------------------------------------
# The header for this section is an example of how to indent subsections by
# adding an additional # symbol.


# Libraries ---------------------------------------------------------------
# Include all the packages that will be used throughout the code.
# This is where packages can be installed if the user does not have them
# currently installed.

# Ensuring the appropriate package versions are used for the project based on
# renv usage
# renv::restore()

# Common Packages
# library(here)
# library(rmarkdown)
# library(shiny)

# tidyverse includes: dplyr, ggplot2, lubridate, purrr, readr, readxl,
# reprex, stringr, tidyr...and more
# See this link for full list: https://tidyverse.tidyverse.org/
# library(tidyverse)

library(readxl)
library(xlsx)
library(dplyr)


# Source Global Functions -------------------------------------------------
# Source scripts that house global functions used within this script.
# Global functions would include functions that are used regularly
# throughout the rest of the script and should make code more readable.

# source()


# Assigning Directory(ies) ------------------------------------------------
# Define variables for frequently used root directories or full directories.
# (This section could be combined with the Constants section.)
# Reminder: avoid using setwd()

## Shared Drive Path (Generic) --------------------------------------------
j_drive <- paste0("//researchsan02b/shr2/deans/Presidents")

# Constants ---------------------------------------------------------------
# Define constants that will be used throughout the code. These are the
# variables that are calculated here and not changed in the rest of the code.

# Data References ---------------------------------------------------------
# (aka Mapping Tables)
# Files that need to be imported for mappings and look-up tables.
# (This section may be combined into the Data Import section.)

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
  mutate(DATE = format(as.Date(DATE), "%m/%d/%Y"),
         START.DATE = format(as.Date(START.DATE), "%m/%d/%Y"),
         END.DATE = format(as.Date(END.DATE), "%m/%d/%Y"))

## Dictionary ------------------------------------------------------------

path_dict_prem <- choose.files(
    default = paste0(j_drive,
                     "/SixSigma/MSHS Productivity/Productivity",
                     "/Volume - Data/MSBI Data/Union Square",
                     "/R Code"),
    caption = "Select DUS Main Dictionary",
    multi = F
  )

dict_epic <- read_xlsx(
    path_dict_prem,
    sheet = 1,
    skip = 0
  )

dict_epic_short <- dict_epic %>%
  select(`Epic Department Name`, `Volume ID`, `Cost Center`, `volume ratio`)

### rehab offsite ---------------------------------------------------------

dict_rehab_docs <- read_xlsx(
  path_dict_prem,
  sheet = 2,
  skip = 0
)

# Data Import -------------------------------------------------------------

path_data_epic <- choose.files(default = j_drive,
                               caption = "Select Epic file",
                               multi = F
                               )

## Skip rows and import ---------------------------------------------------

# The data is not provided in a consistent way.  The number of extra rows
# at the top of the file varies.

skip_ct <- 0
d_check <- 1
skip_ct_max <- 5

while (d_check != 0 & skip_ct < skip_ct_max) {
  col_check <- read_xlsx(path_data_epic, sheet = 1, skip = skip_ct, n_max = 10)
  
  if ("Department" %in% colnames(col_check)) {
    data_raw <- read_xlsx(path_data_epic, sheet = 1, skip = skip_ct)
    d_check <- 0
    rm(col_check)
  } else {
    skip_ct <- skip_ct + 1
  }
}

if (skip_ct == skip_ct_max) {
  stop(paste0("The raw data is not in the appropriate format.\n",
              "Identify the appropriate file and restart")
       )
}


# Creation of Functions --------------------------------------------------
# These are functions that will be commonly used within the rest of the script.
# It might make sense to keep these files in a separate file that is sourced
# in the "Source Global Functions" section above.


# Data Pre-processing -----------------------------------------------------
# Cleaning raw data and ensuring that all values are accounted for such as
# blanks and NA. As well as excluding data that may not be used or needed. This
# section can be split into multiple ones based on the data pre-processing
# needed.
# One of the first steps could be to perform initial checks to make sure data is
# in the correct format.  This might also be done as soon as the data is
# imported.


data_epic <- data_raw %>%
  select(Department, Provider, `Appt Time`, `Epic Department ID`)


## date formatting, check, pay periods -------------------------------------

data_epic <- data_epic %>%
  mutate(`Appt Date` = stringr::str_sub(
    `Appt Time`, 1, nchar("00/00/0000"))
  ) %>%
  mutate(`Appt Date` = format(lubridate::ymd(`Appt Date`), "%m/%d/%Y"))

# if () {
#   date_format <- "%m/%d/%Y"
# } else {
#   date_format <- "%Y-%m-%d"
# }

data_date_min <- min(as.Date(data_epic$`Appt Date`, "%m/%d/%Y"))
data_date_max <- max(as.Date(data_epic$`Appt Date`, "%m/%d/%Y"))

date_ok <- winDialog(type = c("yesno"),
                     message = paste0("The date range for the data is:\n",
                                      data_date_min, "\n",
                                      "to\n",
                                      data_date_max, "\n\n",
                                      "Is this correct?"))

if (date_ok == "NO") {
  stop(paste0("Please get the data for the correct date range.\n",
              "Then restart running this script.")
  )
}

data_epic <- data_epic %>%
  left_join(dict_pay_cycles, by = c("Appt Date" = "DATE"))

## Join Dept & Vol ID -----------------------------------------------------

data_epic_row <- nrow(data_epic)
data_epic <- data_epic %>%
  left_join(dict_epic_short, c("Department" = "Epic Department Name"))
data_epic_row2 <- nrow(data_epic)

winDialog(type = "ok",
          message = paste0("The number of rows in data increased\n",
                           "when joining Volume IDs.\n\n",
                           "This can be expected because of special\n",
                           "rolled-up volumes.\n\n",
                           "The row increase was:\n",
                           data_epic_row2 - data_epic_row, "\n\n",
                           "Press OK to continue."))


## Visit counter ----------------------------------------------------------

# rehab offsite docs that are not included docs get 0
# all others keep their ratio
data_epic <- data_epic %>%
  rename(volume = `volume ratio`) %>%
  mutate(volume = case_when(
    Department %in% dict_rehab_docs$`Epic Department Name` &
      !(Provider %in% dict_rehab_docs$Provider) ~ 0,
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

# would be better to display Cost Center name and the volume IDs
if (length(unique(zero_rows$`Volume ID`)) > 0) {
  winDialog(message = paste0("These volume IDs had a 0 volume:\r",
                             paste(unique(zero_rows$`Volume ID`),
                                   collapse = "; ")))
}

### Combining with upload summary --------------------------------------------
summary_upload <- rbind(summary_upload, zero_rows)

# Data Formatting ---------------------------------------------------------
# How the data will look during the output of the script.
# For example, if you have a data table that needs the numbers to show up as
# green or red depending on whether they meet a certain threshold.

upload_file <- summary_upload %>%
  mutate(entity = "729805",
         facility = "630571",
         budget = "0") %>%
  relocate(c(entity, facility), .before = `Cost Center`)

# Quality Checks ----------------------------------------------------------
# Checks that are performed on the output to confirm data consistency and
# expected outputs.

## new departments --------------------------------------------------------



## volume history ---------------------------------------------------------




# Visualization -----------------------------------------------------------
# How the data will be plotted or how the data table will look including axis
# titles, scales, and color schemes of graphs or data tables.
# (This section may be combined with the Data Formatting section.)


# File Saving -------------------------------------------------------------
# Writing files or data for storage

date_min_char <- format(as.Date(data_date_min, "%m/%d/%Y"), "%Y-%m-%d")
date_max_char <- format(as.Date(data_date_max, "%m/%d/%Y"), "%Y-%m-%d")

file_name_Premier <-
  paste0("MSDUS_Department Volumes_", date_min_char, "_to_", date_max_char,
         ".csv")
path_folder_Premier_export <-
  choose.dir(
    default = j_drive,
    caption = "Select folder to export Premier upload file"
  )
write.table(
  upload_file,
  file = paste0(path_folder_Premier_export, "\\", file_name_Premier),
  row.names = F,
  col.names = F,
  sep = ","
)

# Script End --------------------------------------------------------------