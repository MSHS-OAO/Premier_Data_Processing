# Libraries ---------------------------------------------------------------
library(readxl)
library(xlsx)
library(dplyr)


# Assigning Directories ---------------------------------------------------
J_drive <- paste0("//researchsan02b/shr2/deans/Presidents")


default_folder <-
  paste0(J_drive,"/SixSigma/MSHS Productivity/Productivity/",
         "Volume - Data/MSBI Data/Union Square")

# Importing Dictionaries --------------------------------------------------

## Paycycles --------------------------------------------------------------
dictionary_pay_cycles <- read_xlsx(
  paste0(J_drive, "/SixSigma/MSHS Productivity/Productivity/Universal Data/",
         "Mapping/MSHS_Pay_Cycle.xlsx"),
  col_types = c("date", "date", "date", "skip"))
dictionary_pay_cycles$DATE <- as.Date(dictionary_pay_cycles$DATE)
dictionary_pay_cycles$START.DATE <-
  as.Date(dictionary_pay_cycles$START.DATE)
dictionary_pay_cycles$END.DATE <-
  as.Date(dictionary_pay_cycles$END.DATE)

## Dictionary ------------------------------------------------------------
path_dictionary_Premier_volume <-
  choose.files(
    default = default_folder,
    caption = "Select DUS Main Dictionary",
    multi = F
  )
dictionary_Premier_volume <-
  read_xlsx(
    path_dictionary_Premier_volume,
    sheet = "VolumeID to Cost center # Map",
    col_types = c("guess", "text"),
    col_names = c("Volume ID", "Cost Center"),
    skip = 1
  )

# Epic Dictionaries
# Importing the Dictionaries
dictionary_Epic_department_VolID <-
  read_xlsx(
    path_dictionary_Premier_volume,
    sheet = "New Epic Volume ID Map",
    col_types = c("text", "text", "skip")
  )

# Merging Dictionaries into one
dictionary_EPIC <-
  merge(
    dictionary_Epic_department_VolID,
    dictionary_Premier_volume,
    by.x = "Volume ID",
    by.y = "Volume ID"
  )
# Departments to Remove
remove_departments_Epic <-
  dictionary_Epic_department_VolID[dictionary_Epic_department_VolID$`Volume ID` %in% c("X", "TBD"), "Department"]

# Importing Epic Data -----------------------------------------------------
list_path_data_Epic <-
  as.list(choose.files(
    default = default_folder,
    caption = "Select Epic file(s)",
    multi = T
  ))
list_data_Epic <-
  lapply(list_path_data_Epic, function(x) {
    read_xlsx(path = x, sheet = 1, skip = 2)
  })

# Pre Processing Epic Data ------------------------------------------------
merge_multiple_dataframes <- function(list.dfs) {
  if (length(list.dfs) == 1) {
    return(as.data.frame(list.dfs[1]))
  }
  output <- list.dfs[1]
  for (i in 2:length(list.dfs)) {
    output <-
      merge.data.frame(output, list.dfs[i], all.x = T, all.y = T)
  }
  return(output)
}
data_Epic <- merge_multiple_dataframes(list_data_Epic)
data_Epic$`Appt Date` <-
  unname(sapply(
    data_Epic$Appt.Time,
    FUN = function(x) {
      unlist(strsplit(x, " "))[1]
    }
  )) # separating the Appt Time into Appt Date
data_Epic$`Appt Date` <-
  as.Date(data_Epic$`Appt Date`, tryFormats = "%m/%d/%Y")
data_Epic$Appt.Time <-
  paste(unname(sapply(
    data_Epic$Appt.Time,
    FUN = function(x) {
      unlist(strsplit(x, " "))[2]
    }
  )), unname(sapply(
    data_Epic$Appt.Time,
    FUN = function(x) {
      unlist(strsplit(x, " "))[3]
    }
  ))) # Replace the Appt Time Column with the Hour of the appointment
data_Epic <- arrange(data_Epic, `Appt Date`, `Department`)
data_Epic <-
  merge(
    x = data_Epic,
    y = dictionary_EPIC,
    by = "Department",
    all.x = T
  )
data_Epic <-
  merge(
    x = data_Epic,
    y = dictionary_pay_cycles,
    by.x = "Appt Date",
    by.y = "DATE",
    all.x = T
  )

# Removing Departments

data_Epic_merge <-
  data_Epic[!data_Epic$Department %in% remove_departments_Epic, ]

# remove dates if user chooses
# Epic
choices_date_range_Epic <-
  format(unique(data_Epic$`Appt Date`), "%m/%d/%Y")
remove_dates_Epic <-
  select.list(
    choices = c("None", choices_date_range_Epic),
    title = "Epic:Remove Dates?",
    multiple = T,
    graphics = T,
    preselect = "None"
  )
if (remove_dates_Epic != "None" | is.na(remove_dates_Epic)) {
  remove_dates_Epic <-
    as.Date(remove_dates_Epic, tryFormats = "%m/%d/%Y")
  data_Epic_merge <-
    data_Epic_merge[!(data_Epic_merge$`Appt Date` %in% remove_dates_Epic), ]
} # remove dates if user chooses

# Selecting only needed columns
data_Epic_merge <-
  data_Epic_merge[, c("Cost Center", "Start Date", "End Date", "Volume ID")]
data_Epic_merge$Volume <-
  rep(1, length(data_Epic_merge$`Cost Center`))
colnames(data_Epic_merge) <-
  c("Cost Center", "Start Date", "End Date", "Volume ID", "Volume")

# Used to have the option to merge eIDX/IDX data with Epic data.
# No longer necessary.  For simplicity, just reassign data to the variable
# used in the rest of the script
  data_visits <- data_Epic_merge

# Removing NA Vol IDs and Cost Centers
remove_NA_vol <- data_visits[which(is.na(data_visits$VolumeID)), ]
if (length(remove_NA_vol$VolumeID) != 0) {
  data_visits <-
    data_visits[!data_visits$VolumeID %in% remove_NA_vol$VolumeID, ]
}
remove_NA_CC <-
  data_visits[which(is.na(data_visits$`Cost Center`)), ]
if (length(remove_NA_CC$`Cost Center`) != 0) {
  data_visits <-
    data_visits[!data_visits$`Cost Center` %in% remove_NA_CC$`Cost Center`, ]
}


# Rehab off-site prep -----------------------------------------------------


rehab_offs_ratio <- 0.34
rehab_offs_docs <- c("SPINNER, DAVID A", "CHANG, RICHARD G")
rehab_offs_dept <- c("300 CADMAN PLAZA REHAB MED",
                     "309 W 23RD ST REHAB FLUOROSCOPY")
rehab_offs_cc <- "407000040421109"
rehab_offs_vol_id <- "407404211092"

data_rehab_offs <- data_Epic %>%
  filter(Provider %in% rehab_offs_docs,
         Department %in% rehab_offs_dept) %>%
    mutate(`Volume ID` = rehab_offs_vol_id,
           `Cost Center` = rehab_offs_cc)

if (remove_dates_Epic != "None" | is.na(remove_dates_Epic)) {
  remove_dates_Epic <-
    as.Date(remove_dates_Epic, tryFormats = "%m/%d/%Y")
  data_rehab_offs <-
    data_rehab_offs[!(data_Epic_merge$`Appt Date` %in% remove_dates_Epic), ]
} # remove dates if user chooses

# Selecting only needed columns
data_rehab_offs <-
  data_rehab_offs[, c("Cost Center", "Start Date", "End Date", "Volume ID")]
data_rehab_offs$Volume <-
  rep(rehab_offs_ratio, length(data_rehab_offs$`Cost Center`))
colnames(data_rehab_offs) <-
  c("Cost Center", "Start Date", "End Date", "Volume ID", "Volume")


# Merge Offsite with Main Visits ------------------------------------------

data_visits <- rbind(data_visits, data_rehab_offs)

# Creating Premier Upload -------------------------------------------------
# Created needed columns
data_visits$`Entity ID` <-
  rep("729805", length(data_visits$`Cost Center`))
data_visits$`Facility ID` <-
  rep("630571", length(data_visits$`Cost Center`))
data_visits$Budget <- rep("0", length(data_visits$`Cost Center`))
# Aggregating Data
data_visits <-
  aggregate(
    data_visits$Volume,
    by = list(
      data_visits$`Entity ID`,
      data_visits$`Facility ID`,
      data_visits$`Cost Center`,
      data_visits$`Start Date`,
      data_visits$`End Date`,
      data_visits$`Volume ID`,
      data_visits$Budget
    ),
    FUN = "sum"
  )
colnames(data_visits) <-
  c(
    "Corporate ID",
    "Facility ID",
    "Cost Center",
    "Start Date",
    "End Date",
    "Volume ID",
    "Budget",
    "Volume"
  )
# Formatting Data
data_visits <-
  arrange(data_visits, `Start Date`, `End Date`, `Cost Center`, `Volume ID`)
data_visits <-
  subset(
    data_visits,
    select = c(
      "Corporate ID",
      "Facility ID",
      "Cost Center",
      "Start Date",
      "End Date",
      "Volume ID",
      "Volume",
      "Budget"
    )
  )
data_visits$`Start Date` <-
  format(data_visits$`Start Date`, "%m/%d/%Y")
data_visits$`End Date` <- format(data_visits$`End Date`, "%m/%d/%Y")

data_visits$Volume <- round(data_visits$Volume, 0)

# Exporting Premier Upload File -------------------------------------------
file_name_Premier <-
  paste0(
    "MSDUS_Department Volumes_",
    format(min(
      as.Date(data_visits$`Start Date`, "%m/%d/%Y")
    ), "%d%b%Y"),
    " to ",
    format(max(
      as.Date(data_visits$`End Date`, "%m/%d/%Y")
    ), "%d%b%Y"),
    ".csv"
  )
path_folder_Premier_export <-
  choose.dir(default = default_folder, caption = "Select folder to export Premier upload file")
write.table(
  data_visits,
  file = paste0(path_folder_Premier_export, "\\", file_name_Premier),
  row.names = F,
  col.names = F,
  sep = ","
)
