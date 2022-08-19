# Libraries ---------------------------------------------------------------
library(readxl)
library(xlsx)
library(dplyr)


# Assigning Directories ---------------------------------------------------
J_drive <- paste0("//researchsan02b/shr2/deans/Presidents")


default_folder <-
  paste0(
    J_drive, "/SixSigma/MSHS Productivity/Productivity/",
    "Volume - Data/MSBI Data/Union Square"
  )

# Importing Dictionaries --------------------------------------------------

## Pay cycles -------------------------------------------------------------
dict_pay_cycles <- read_xlsx(
  paste0(
    J_drive, "/SixSigma/MSHS Productivity/Productivity/Universal Data/",
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

# improvement: create a single dictionary to import

path_dict_prem_vol <-
  choose.files(
    default = default_folder,
    caption = "Select DUS Main Dictionary",
    multi = F
  )

dict_prem_vol <-
  read_xlsx(
    path_dict_prem_vol,
    sheet = "VolumeID to Cost center # Map",
    col_types = c("guess", "text"),
    col_names = c("Volume ID", "Cost Center"),
    skip = 1
  )

# Epic Dictionaries
# Importing the Dictionaries
dict_Epic_dept_VolID <-
  read_xlsx(
    path_dict_prem_vol,
    sheet = "New Epic Volume ID Map",
    col_types = c("text", "text", "skip")
  )

# Merging Dictionaries into one
dict_EPIC <-
  merge(
    dict_Epic_dept_VolID,
    dict_prem_vol,
    by.x = "Volume ID",
    by.y = "Volume ID"
  )

# path_dict_Prem <- choose.files(
#     default = default_folder,
#     caption = "Select DUS Main Dictionary",
#     multi = F
#   )
#
# dict_EPIC2 <- read_xlsx(
#     path_dict_Prem,
#     sheet = 1,
#     skip = 0
#   )
#
# dict_EPIC2 <- dict_EPIC2 %>%
#   select(`Epic Department Name`,
#          `Volume ID`,
#          `Cost Center`)

# # Departments to Remove
# remove_departments_Epic <- dict_Epic_dept_VolID %>%
#   filter(`Volume ID` %in% c("X", "TBD")) %>%
#   select(Department)

# Importing Epic Data -----------------------------------------------------
path_data_Epic <- choose.files(default = default_folder,
                               caption = "Select Epic file",
                               multi = F
                               )

data_raw <- read_xlsx(path = path_data_Epic, sheet = 1, skip = 1)
# improve this segment by identifying the proper number of rows to skip
# compare column names with the previous upload?  Or compare with the
#  minimum columns needed?

data_raw <- data_raw %>%
  select(Department, Provider, `Appt Time`, `Epic Department ID`)

# Pre Processing Epic Data ------------------------------------------------

## Date Formatting ---------------------------------------------------------

# insert if statements to modify date/time columns based on the
# input data format
# use tidyverse and lubridate to handle the time/date??

data_Epic <- data_raw %>%
  mutate(`Appt Date` = stringr::str_sub(
    `Appt Time`, 1, nchar("00/00/0000"))
  )

data_min_date <- min(as.Date(data_Epic$`Appt Date`, "%m/%d/%Y"))
data_max_date <- max(as.Date(data_Epic$`Appt Date`, "%m/%d/%Y"))

# improvement: prompt user to determine if date range is appropriate

## Join IDs and pay cycles -------------------------------------------------

data_Epic <- data_Epic %>%
  left_join(dict_EPIC) %>%
  left_join(dict_pay_cycles, by = c("Appt Date" = "DATE"))

## New Dept Check ------------------------------------------------------

new_dept <- data_Epic %>%
  filter(!(Department %in% dict_EPIC$Department)) %>%
  select(Department) %>%
  unique() # %>%
  # sort() # some issue with sorting the data frame

# consider where this filtering is best positioned
# using the updated dictionary will improve this situation

# if there are new depts that should be incorporated, a warning gives
# the user an opporunity to check this before continuing
# if(length(unique(new_dept$Department)) > 0) {
#   new_dept_stop <- winDialog(
#     message = paste0("These Departments are not in the dictionary:\r",
#                      paste(unique(zero_rows$`Volume ID`), collapse = "; "),
#                      "\r",
#                      "To stop running this script, press \"Cancel\" \r",
#                      "Press OK to continue"),
#     type = "okcancel")
# }
#
# if(new_dept_stop == "CANCEL") {
#   stop("Fix the dept dictionary if you like based on the new departments")
# }

## Rehab off-site prep -----------------------------------------------------

### Constants --------------------------------------------------------------

rehab_offs_ratio <- 0.34
rehab_offs_docs <- c("SPINNER, DAVID A", "CHANG, RICHARD G")
rehab_offs_dept <- c("300 CADMAN PLAZA REHAB MED",
                     "309 W 23RD ST REHAB FLUOROSCOPY"
                     )
rehab_offs_cc <- "407000040421109"
rehab_offs_vol_id <- "407404211092"

### Assigning Cost Center and Vol ID ---------------------------------------

data_Epic <- data_Epic %>%
  mutate(
    `Volume ID` = case_when(
      Provider %in% rehab_offs_docs &
        Department %in% rehab_offs_dept ~ rehab_offs_vol_id,
      TRUE ~ `Volume ID`
    ),
    `Cost Center` = case_when(
      Provider %in% rehab_offs_docs &
        Department %in% rehab_offs_dept ~ rehab_offs_cc,
      TRUE ~ `Cost Center`
    )
  )

## Appending volume counter -----------------------------------------------

data_Epic <- data_Epic %>%
  mutate(
    Volume = case_when(
      `Volume ID` == rehab_offs_vol_id ~ rehab_offs_ratio,
      TRUE ~ 1
    )
  )

## Summarizing ------------------------------------------------------------

data_visits <- data_Epic %>%
  group_by(`Cost Center`, START.DATE, END.DATE, `Volume ID`) %>%
  summarize(Visits = sum(Volume)) %>%
  mutate(Visits = round(Visits, digits = 0)) %>%
  filter(!is.na(`Volume ID`))

# Documenting the depts included that are not going into Premier
data_visits_NA_dept <- data_Epic %>%
  filter(!(Department %in% dict_EPIC$Department) & is.na(`Volume ID`)) %>%
  group_by(Department, START.DATE, END.DATE) %>%
  summarize(Visits = sum(Volume))
# the filter will change when using the new dictionary
# if the value of Volume ID is X or TBD then it should also be included


## Endo Special Roll-up prep -----------------------------------------------
# this has to occur after the summarization because of the additional
# roll-up tha thas to occur

### Constants --------------------------------------------------------------
endo_ru_cc <- "401000040412828"
endo_ru_vol_id <- "401404128281"

endo_ind_vol_id <- c("401404128282", "401404128283", "414403128371")

# this step could be skipped if the dictionary were to have multiple mappings
# for the endo depts that get rolled up

### Processing -------------------------------------------------------------

data_visits_endo <- data_Epic %>%
  filter(`Volume ID` %in% endo_ind_vol_id) %>%
  group_by(START.DATE, END.DATE) %>%
  summarize(Visits = sum(Volume)) %>%
  mutate(`Cost Center` = endo_ru_cc, .before = START.DATE) %>%
  mutate(`Volume ID` = endo_ru_vol_id, .before = Visits)

### Combining with Visits data --------------------------------------------
data_visits <- rbind(data_visits, data_visits_endo)

## Add rows for volume ID with 0 volume ------------------------------------

data_dates <- data_visits %>%
  ungroup() %>%
  select(START.DATE, END.DATE) %>%
  unique()

dict_Epic_unique <- dict_EPIC %>%
  select(`Cost Center`, `Volume ID`) %>%
  unique()

dict_and_date <- merge(data_dates, dict_Epic_unique)

missing_vol_id_date <- dict_and_date %>%
  anti_join(data_visits)

zero_rows <- missing_vol_id_date %>%
  mutate(Visits = 0) %>%
  relocate(`Cost Center`, .before = START.DATE)

# would be better to display Cost Center name and the volume IDs
if (length(unique(zero_rows$`Volume ID`)) > 0) {
  winDialog(message = paste0("These volume IDs had a 0 volume:\r",
                             paste(unique(zero_rows$`Volume ID`),
                                   collapse = "; ")))
}

### Combining with Visits data --------------------------------------------
data_visits <- rbind(data_visits, zero_rows)


# Creating Premier Upload -------------------------------------------------

data_visits <- data_visits %>%
  mutate(entity = "729805",
         facility = "630571",
         budget = "0") %>%
  relocate(c(entity, facility), .before = `Cost Center`)

# Exporting Premier Upload File -------------------------------------------
min_date_char <- format(as.Date(data_min_date, "%m/%d/%Y"), "%Y-%m-%d")
max_date_char <- format(as.Date(data_max_date, "%m/%d/%Y"), "%Y-%m-%d")

file_name_Premier <-
  paste0("MSDUS_Department Volumes_", min_date_char, "_to_", max_date_char,
         ".csv")
path_folder_Premier_export <-
  choose.dir(
    default = default_folder,
    caption = "Select folder to export Premier upload file"
  )
write.table(
  data_visits,
  file = paste0(path_folder_Premier_export, "\\", file_name_Premier),
  row.names = F,
  col.names = F,
  sep = ","
)
