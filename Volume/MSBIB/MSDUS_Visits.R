# Libraries ---------------------------------------------------------------

library(readxl)
library(xlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(writexl)
# Assigning Directory(ies) ------------------------------------------------

## Shared Drive Path (Generic) --------------------------------------------
j_drive <- paste0("//researchsan02b/shr2/deans/Presidents")


# Data References ---------------------------------------------------------

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


### rehab offsite ---------------------------------------------------------

dict_rehab_docs <- read_xlsx(
  path_dict_prem,
  sheet = 2,
  skip = 0
)


# Data Import -------------------------------------------------------------

path_data_epic <- choose.files(
  default = paste0(j_drive,
                   "/SixSigma/MSHS Productivity/Productivity",
                   "/Volume - Data/MSBI Data/Union Square",
                   "/Source Data"),
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
  
  if ("Department" %in% colnames(col_check) |
      "DEPARTMENT_NAME" %in% colnames(col_check)) {
    data_raw <- read_xlsx(path_data_epic, sheet = 1, skip = skip_ct)
    d_check <- 0
    rm(col_check)
  } else {
    skip_ct <- skip_ct + 1
  }
}
# alternatively, to help with date handling, could bring all columns
# in as character strings


if (skip_ct == skip_ct_max) {
  stop(paste0("The raw data is not in the appropriate format.\n",
              "Identify the appropriate file and restart")
  )
}

# if the atypical file format has been provided, the column names
# need to be changed in order for the rest of the script to work
if ("DEPARTMENT_NAME" %in% colnames(data_raw)) {
  data_raw <- data_raw %>%
    rename(Department = DEPARTMENT_NAME,
           Provider = PROV_NAME,
           `Appt Time` = APPT_TIME)
  
  # if there are NA dates then Date of Service can be used instead
  data_raw <- data_raw %>%
    mutate(`Appt Time` = case_when(
      is.na(`Appt Time`) ~ DATE_OF_SERVICE,
      TRUE ~ `Appt Time`
    ))
}


# Data Pre-processing -----------------------------------------------------

data_epic <- data_raw %>%
  select(Department, Provider, `Appt Time`) %>%
  filter(! is.na(`Appt Time`))


## date formatting, check, pay periods -------------------------------------

data_epic <- data_epic %>%
  mutate(`Appt Date` = stringr::str_sub(
    `Appt Time`, 1, nchar("00/00/0000"))
  )

if ("POSIXct" %in% class(data_epic$`Appt Time`)) {
  data_epic <- data_epic %>%
    mutate(`Appt Date` = format(lubridate::ymd(`Appt Date`), "%m/%d/%Y"))
}
# alternatively, could bring all columns of raw data as character

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


# Quality Checks ----------------------------------------------------------

## new departments --------------------------------------------------------

new_dept <- data_epic %>%
  filter(is.na(volume)) %>%
  select(Department) %>%
  unique()

if (length(new_dept$Department) > 0) {
  new_dept_stop <- winDialog(
    message = paste0("These Departments are not in the dictionary:\r\r",
                     paste(unique(new_dept$Department), collapse = "\n\n"),
                     "\r\r",
                     "Press \"OK\" to continue\r",
                     "Press \"Cancel\" to stop running this script"),
    type = "okcancel")
  
  if (new_dept_stop == "CANCEL") {
    stop(paste0("Incorporate the new depts into the dictionary as desired.\n",
                "Restart the script after the dictionary is as desired.")
    )
  }
  
} else {
  new_dept_stop <- "OK"
  message("No new departments identified.")
}

# Visualization -----------------------------------------------------------

#add note column to the upload file so it can bind to trend data file
new_trend_data <- upload_file %>%
  mutate(Note = NA) %>%
  mutate(START.DATE = mdy(START.DATE)) %>%
  mutate(START.DATE = as.Date(START.DATE, format = "%Y-%m-%d")) %>%
  mutate(END.DATE = mdy(END.DATE)) %>%
  mutate(END.DATE = as.Date(END.DATE, format = "%Y-%m-%d"))

#read in trend data file
old_trend_data <- read_xlsx(paste0(j_drive, "/SixSigma/MSHS Productivity",
                                   "/Productivity/Volume - Data/MSBI Data",
                                   "/Union Square/Calculation Worksheets",
                                   "/MSDUS_trend_data.xlsx"),
                            sheet = 1) %>%
  mutate(START.DATE = as.Date(START.DATE)) %>%
  mutate(END.DATE = as.Date(END.DATE)) %>%
  mutate(`Volume ID` = as.character(`Volume ID`))

#combine new upload data with the existing trend data
updated_trend_data <- rbind(old_trend_data, new_trend_data) %>%
  mutate(START.DATE = as.Date(START.DATE)) %>%
  mutate(END.DATE = as.Date(END.DATE)) %>%
  unique()

#overwrite trend data file with updated trend data
write_xlsx(updated_trend_data, paste0(j_drive, "/SixSigma/MSHS Productivity",
                                      "/Productivity/Volume - Data/MSBI Data",
                                      "/Union Square/Calculation Worksheets",
                                      "/MSDUS_trend_data.xlsx"))

#read in MSDUS trend groups table for a left join
trend_groups <- read_xlsx(paste0(j_drive, "/SixSigma/MSHS Productivity",
                                 "/Productivity/Volume - Data/MSBI Data",
                                 "/Union Square/Calculation Worksheets",
                                 "/MSDUS_trend_groups.xlsx")) %>%
  mutate(`Volume ID` = as.character(`Volume ID`))

#prepare the updated trend data for visualization
plot_trend_data <- updated_trend_data %>%
  left_join(trend_groups,
            by = c("Volume ID" = "Volume ID")) %>%
  arrange(desc(START.DATE)) %>%
  mutate(NEW.NAME = paste0(`Cost Center Description`,
                           " - ",
                           as.character(`Volume ID`))) %>%
  filter(START.DATE >= today() - 180)

#creating multiple-bar plots (x = volume ID, y = volume, and
#each bar indicates the volume on a specific end date)
#each plot is depicts sets of volume ids with similar visit counts
for (i in c("low", "med", "high")){
  plot <- ggplot(data = filter(plot_trend_data, `Plot Group` == i),
         mapping = aes(x = `NEW.NAME`, y = `visits`, fill = `END.DATE`)) +
    geom_bar(position = "dodge2", stat = "identity") +
    coord_flip() + 
    labs(y = "Visits per Pay Period", x = "Cost Center & Volume ID") + 
    ggtitle(paste0("MSDUS Visit Trends: ", i, " volume group"))
  print(plot)
}
# File Saving -------------------------------------------------------------

date_min_char <- format(as.Date(data_date_min, "%m/%d/%Y"), "%Y-%m-%d")
date_max_char <- format(as.Date(data_date_max, "%m/%d/%Y"), "%Y-%m-%d")

file_name_premier <-
  paste0("MSDUS_Department Volumes_", date_min_char, "_to_", date_max_char,
         ".csv")
path_folder_premier_export <- paste0(j_drive,
                                     "/SixSigma/MSHS Productivity/Productivity",
                                     "/Volume - Data/MSBI Data/Union Square",
                                     "/Calculation Worksheets")
write.table(
  upload_file,
  file = paste0(path_folder_premier_export, "\\", file_name_premier),
  row.names = F,
  col.names = F,
  sep = ","
)

message(paste0("\nUpload file written to:\n",
               paste0(path_folder_premier_export, "\\", file_name_premier,
                      "\n")))

# Script End --------------------------------------------------------------
