# Libraries ---------------------------------------------------------------

library(readxl)
library(xlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(writexl)

library(rstudioapi)
library(DBI)
library(odbc)
library(dplyr)
library(dbplyr)
library(glue)


# WORKBENCH ---------------------------------------------------------------

showDialog(
  title = "Workbench Reminder",
  message = paste0("Those script must be run on Workbench.  ",
                   "If running from desktop, please stop and ",
                   "open on Workbench.")
)

# Assigning Directory(ies) ------------------------------------------------

## Shared Drive Path --------------------------------------------
j_drive <- paste0("/SharedDrive/deans/Presidents/")

dir_universal <- paste0(j_drive, "SixSigma/MSHS Productivity",
                        "/Productivity/Universal Data/")

# Data References ---------------------------------------------------------

## Pay cycles -------------------------------------------------------------

oao_con <- dbConnect(odbc(), "OAO Cloud DB Production")

dict_pay_cycles <- tbl(oao_con, "LPM_MAPPING_PAYCYCLE") %>% collect()

dict_PC_raw <- dict_pay_cycles %>%
  rename(DATE = PAYCYCLE_DATE,
         START.DATE = PP_START_DATE,
         END.DATE = PP_END_DATE,
         PREMIER.DISTRIBUTION = PREMIER_DISTRIBUTION)

# modifying column names in order to not have to recode the rest of the script
# dates originally come in as POSIXct, so they're being converted to Date
dict_pay_cycles <- dict_pay_cycles %>%
  rename(DATE = PAYCYCLE_DATE,
         START.DATE = PP_START_DATE,
         END.DATE = PP_END_DATE,
         PREMIER.DISTRIBUTION = PREMIER_DISTRIBUTION) %>%
  mutate(DATE = format(as.Date(DATE), "%m/%d/%Y"),
         START.DATE = format(as.Date(START.DATE), "%m/%d/%Y"),
         END.DATE = format(as.Date(END.DATE), "%m/%d/%Y")) %>%
  select(-PREMIER.DISTRIBUTION)


### Date Selection -------------------------------------------------------
# select the date range to pull

#Table of distribution dates earlier than the current date
dist_dates <- dict_PC_raw %>%
  select(END.DATE, PREMIER.DISTRIBUTION) %>%
  distinct() %>%
  # drop_na() %>%
  arrange(END.DATE) %>%
  filter(PREMIER.DISTRIBUTION %in% c(TRUE, 1),
         END.DATE < as.POSIXct(Sys.Date()))


#Selecting the most recent distribution date
pp.end <- max(dist_dates$END.DATE)
pp.start <- dist_dates %>%
  arrange(END.DATE) %>%
  select(END.DATE)
pp.start <- pp.start$END.DATE[nrow(pp.start) - 1] + lubridate::days(1)

#Confirming date range
answer <- showQuestion(
  title = "Date Range",
  message = paste0(
    "Date range will be: ", pp.start, " to ", pp.end, ".        ",
    "If this is correct, press OK.  ",
    "If this is not correct, press Cancel and ",
    "you will be prompted to provide the correct date range."))

if (answer == FALSE) {
  pp.start <- as.Date(
    rstudioapi::showPrompt(
      title = "pp.start input",
      message = paste0("What is the date you'd like the data to start from?\r",
                       "\rPlease enter the date in YYYY-MM-DD format")))
  pp.end <- as.Date(
    rstudioapi::showPrompt(
      title = "pp.end input",
      message = paste0("What is the date you'd like the data to go up to?\r",
                       "\rPlease enter the date in YYYY-MM-DD format")))
}

if (pp.start > pp.end) {
  showDialog(
    title = "date errors",
    message = paste0("The start date you entered is after the end date.  ",
                     "You can expect errors to arise later in the script.")
  )
}

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

epic_dpts <- unique(dict_epic_short$`Epic Department Name`)

### rehab offsite ---------------------------------------------------------

dict_rehab_docs <- read_xlsx(
  path_dict_prem,
  sheet = 2,
  skip = 0
)


# Data Import -------------------------------------------------------------

# set up connection to schema
con <- dbConnect(odbc(), "OAO Cloud DB Armando")

pp.start.fmt <- format(pp.start, "%Y-%m-%d")
pp.end.fmt <- format(pp.end, "%Y-%m-%d")

data_raw <- tbl(con,
                in_schema("VILLEA04", "AMBULATORY_ACCESS_VIEW")) %>%
  filter(DEPARTMENT %in% epic_dpts,
         APPT_DATE_YEAR >= as.Date(pp.start.fmt),
         APPT_DATE_YEAR <= as.Date(pp.end.fmt)) %>%
  select(DEPARTMENT, DEPARTMENT_ID, APPT_DATE_YEAR, APPT_STATUS, PROVIDER) %>%
  filter(APPT_STATUS %in% c("Completed", "Arrived",
                            "Checked in", "Checked out")) %>%
  show_query() %>%
  collect()

# Data Pre-processing -----------------------------------------------------

## date formatting, check, pay periods -------------------------------------

data_epic <- data_raw %>%
  left_join(dict_pay_cycles %>%
              mutate(
                DATE = as.Date(DATE, format = "%m/%d/%Y")
              ),
            by = c("APPT_DATE_YEAR" = "DATE"))


## Join Dept & Vol ID -----------------------------------------------------

data_epic_row <- nrow(data_epic)
data_epic <- data_epic %>%
  left_join(dict_epic_short, by = c("DEPARTMENT" = "Epic Department Name"))
data_epic_row2 <- nrow(data_epic)

showDialog(title = "Row Increase",
           message = paste0("The number of rows in data increased ",
                            "when joining Volume IDs.  ",
                            "This can be expected because of roll-up volumes ",
                            "that were created for a few departments.  ",
                            "The row increase was: ",
                            data_epic_row2 - data_epic_row, ".  ",
                            "Press OK to continue."))


## Visit counter ----------------------------------------------------------

# rehab offsite docs that are not included docs get 0
# all others keep their ratio
data_epic <- data_epic %>%
  rename(volume = `volume ratio`) %>%
  mutate(volume = case_when(
    DEPARTMENT %in% dict_rehab_docs$`Epic Department Name` &
      !(PROVIDER %in% dict_rehab_docs$Provider) ~ 0,
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
  showDialog(
    title = "Zero Depts",
    message = paste0(
      "The following Depts had a pay period with 0 volume:  ",
      paste(sort(unique(zero_depts$`Epic Department Name`)),
            collapse = " | "))
  )
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

## Visualization -----------------------------------------------------------
#consolidate plot groups into the MSUS Epic Dict
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

#format dict_epic for a left join
trend_groups <- dict_epic %>%
  select("Volume ID", "Plot Group", "Cost Center Name") %>%
  mutate(`Volume ID` = as.character(`Volume ID`)) %>%
  na.omit() %>%
  unique()

#prepare the updated trend data for visualization
plot_trend_data <- updated_trend_data %>%
  left_join(trend_groups,
            by = c("Volume ID" = "Volume ID")) %>%
  arrange(desc(START.DATE)) %>%
  mutate(NEW.NAME = paste0(`Cost Center Name`,
                           " - ",
                           as.character(`Volume ID`))) %>%
  filter(START.DATE >= today() - 180)

#creating multiple-bar plots (x = volume ID, y = volume, and
#each bar indicates the volume on a specific end date)
#each plot is depicts sets of volume ids with similar visit counts
for (i in c("low", "med", "high")) {
  plot <- ggplot(data = filter(plot_trend_data, `Plot Group` == i),
                 mapping = aes(x = `NEW.NAME`, y = `visits`, fill = `END.DATE`)) +
    geom_bar(position = "dodge2", stat = "identity") +
    coord_flip() +
    labs(y = "Visits per Pay Period", x = "Cost Center & Volume ID") +
    ggtitle(paste0("MSDUS Visit Trends: ", i, " volume group"))
  print(plot)
}
# File Saving -------------------------------------------------------------

date_min_char <- format(pp.start, "%Y-%m-%d")
date_max_char <- format(pp.end, "%Y-%m-%d")

file_name_premier <-
  paste0("MSDUS_Department Volumes_", date_min_char, "_to_", date_max_char,
         ".csv")
path_folder_premier_export <- paste0(j_drive,
                                     "/SixSigma/MSHS Productivity/Productivity",
                                     "/Volume - Data/MSBI Data/Union Square",
                                     "/Calculation Worksheets")

upload_cols <- c("Corporation Code",
                 "Entity Code",
                 "Cost Center Code",
                 "Start Date",
                 "End Date",
                 "Volume Code",
                 "Actual Volume",
                 "Budget Volume")

colnames(upload_file) <- upload_cols

write.table(
  upload_file,
  file = paste0(path_folder_premier_export, "/", file_name_premier),
  row.names = FALSE,
  col.names = TRUE,
  sep = ","
)

message(paste0("\nUpload file written to:\n",
               paste0(path_folder_premier_export, "/", file_name_premier,
                      "\n")))

# Script End --------------------------------------------------------------
