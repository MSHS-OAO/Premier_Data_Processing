# Loading Libraries -------------------------------------------------------
library(readxl)
library(xlsx)
library(dplyr)

# Importing Dictionaries --------------------------------------------------
default_folder <-
  "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSBI Data/Union Square"

if (exists("Pay Cycles.xlsx")) {
  dictionary_pay_cylces <-
    read_xlsx(
      "Pay Cycles.xlsx",
      sheet = 1,
      col_types = c(
        "date",
        "skip",
        "skip",
        "skip",
        "skip",
        "skip",
        "skip",
        "skip",
        "skip",
        "skip",
        "date",
        "date",
        "skip"
      )
    )
} else {
  path_dictionary_pay_cylces <-
    choose.files(
      default = default_folder,
      caption = "Select Pay Cycles File",
      multi = F
    )
  dictionary_pay_cylces <-
    read_xlsx(
      path_dictionary_pay_cylces,
      sheet = 1,
      col_types = c(
        "date",
        "skip",
        "skip",
        "skip",
        "skip",
        "skip",
        "skip",
        "skip",
        "skip",
        "skip",
        "date",
        "date",
        "skip"
      )
    )
}
dictionary_pay_cylces$Date <- as.Date(dictionary_pay_cylces$Date)
dictionary_pay_cylces$`Start Date` <-
  as.Date(dictionary_pay_cylces$`Start Date`)
dictionary_pay_cylces$`End Date` <-
  as.Date(dictionary_pay_cylces$`End Date`)
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

# eIDX/IDX Dictionaries
# Rollup + Department Map for Visits:
dictionary_eIDX_rollup_department <-
  read_xlsx(
    path_dictionary_Premier_volume,
    sheet = "Sch Loc to Dept Map visit e.IDX",
    col_types = c("skip", "skip", "text", "text", "text", "skip", "skip", "skip")
  )
# Volume ID (Premier),Type, and eIDX/idx Department Map:
dictionary_eIDX_departments <-
  read_xlsx(
    path_dictionary_Premier_volume,
    sheet = "New eIDX visit - volID map",
    col_types = c("text", "text", "text", "skip")
  )
# Merging volID/departnent map with the cost center map
dictionary_eIDX <-
  merge(
    x = dictionary_eIDX_departments,
    y = dictionary_Premier_volume,
    by.x = "VolumeID",
    by.y = "Volume ID",
    all.x = T
  )
# Departments to Remove
remove_departments_eIDX <-
  read_xlsx(path_dictionary_Premier_volume, sheet = "eIDX_IDX Departments to Remove")
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

# Importing eIDX/IDX Data -------------------------------------------------
answer_eIDX <-
  select.list(
    choices = c("Yes", "No"),
    title = "Is there an eIDX/IDX file?",
    multiple = F,
    graphics = T
  )
if (answer_eIDX == "Yes") {
  path_eIDX <-
    choose.files(
      default = default_folder,
      caption = "Select the eIDX/IDX file",
      multi = F
    )
  choices_eIDX_sheets <- c(excel_sheets(path_eIDX), "None")
  sheet_eIDX <-
    select.list(
      choices = choices_eIDX_sheets,
      title = "Select eIDX Arrived Visits Sheet",
      graphics = T
    )
  if (sheet_eIDX == "None") {
    data_eIDX_visits <- NA
  } else {
    data_eIDX_visits <-
      as.matrix(read_xlsx(path = path_eIDX, sheet = sheet_eIDX))
  }
  sheet_IDX <-
    select.list(
      choices = choices_eIDX_sheets,
      title = "Select IDX Arrived Visits Sheet",
      graphics = T
    )
  if (sheet_IDX == "None") {
    data_IDX_visits <- NA
  } else {
    data_IDX_visits <-
      as.matrix(read_xlsx(path = path_eIDX, sheet = sheet_IDX))
  }
  # importing data
  if (length(data_eIDX_visits) == 1 | length(data_IDX_visits == 1)) {
    if (length(data_IDX_visits) == 1 & length(data_eIDX_visits) != 1) {
      data_eIDXIDX_visits <- as.data.frame(data_eIDX_visits)
    } else if (length(data_IDX_visits) != 1 &
      length(data_eIDX_visits) != 1) {
      data_eIDXIDX_visits <- as.data.frame(data_IDX_visits)
    } else {
      data_eIDXIDX_visits <-
        merge.data.frame(data_eIDX_visits,
          data_IDX_visits,
          all.x = T,
          all.y = T
        )
    }
  } # merging data from eIDX/IDX

  # Pre Processing eIDX/IDX Data --------------------------------------------
  data_eIDXIDX_visits$`Sch Visit Num` <-
    as.numeric(as.character(data_eIDXIDX_visits$`Sch Visit Num`))
  data_eIDXIDX_visits$`SchDateId Date (MM/DD/YYYY)` <-
    as.Date(data_eIDXIDX_visits$`SchDateId Date (MM/DD/YYYY)`,
      tryFormats = "%m/%d/%Y"
    )
  data_eIDXIDX_visits$`Sch SchDept` <-
    as.character(data_eIDXIDX_visits$`Sch SchDept`)
  data_eIDXIDX_visits$`Sch SchDeptSch SchLoc` <-
    paste0(
      data_eIDXIDX_visits$`Sch SchDept`,
      data_eIDXIDX_visits$`Sch SchLoc`
    )
  data_eIDXIDX_visits <-
    arrange(
      data_eIDXIDX_visits,
      `SchDateId Date (MM/DD/YYYY)`,
      `Sch SchDeptSch SchLoc`
    ) # sorting data
  data_eIDXIDX_visits <-
    merge(
      x = data_eIDXIDX_visits,
      y = dictionary_eIDX_rollup_department,
      by = "Sch SchDeptSch SchLoc",
      all.x = T
    )
  data_eIDXIDX_visits <-
    merge(
      data_eIDXIDX_visits,
      subset(
        dictionary_eIDX,
        select = c("Department", "VolumeID", "Cost Center")
      ),
      by = "Department",
      all.x = T
    )
  data_eIDXIDX_visits <-
    merge(
      x = data_eIDXIDX_visits,
      y = dictionary_pay_cylces,
      by.x = "SchDateId Date (MM/DD/YYYY)",
      by.y = "Date",
      all.x = T
    )
}
# Importing Epic Data -----------------------------------------------------
list_path_data_Epic <-
  as.list(choose.files(
    default = default_folder,
    caption = "Select Epic file(s)",
    multi = T
  ))
list_data_Epic <-
  lapply(list_path_data_Epic, function(x) {
    read_xlsx(path = x, sheet = 2, skip = 0)
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
# data_Epic$`Appt Date` <-
#   unname(sapply(
#     data_Epic$Appt.Time,
#     FUN = function(x) {
#       unlist(strsplit(x, " "))[1]
#     }
#   )) # separating the Appt Time into Appt Date
data_Epic$Appt.Date <-
  as.Date(data_Epic$Appt.Date, tryFormats = "%m/%d/%Y")
# data_Epic$Appt.Time <-
#   paste(unname(sapply(
#     data_Epic$Appt.Time,
#     FUN = function(x) {
#       unlist(strsplit(x, " "))[2]
#     }
#   )), unname(sapply(
#     data_Epic$Appt.Time,
#     FUN = function(x) {
#       unlist(strsplit(x, " "))[3]
#     }
#   ))) # Replace the Appt Time Column with the Hour of the appointment
data_Epic <- arrange(data_Epic, Appt.Date, `Department`)
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
    y = dictionary_pay_cylces,
    by.x = "Appt.Date",
    by.y = "Date",
    all.x = T
  )

# Merging Data from Epic & eIDX/IDX ------------------------------------------------
# Removing Departments

if (answer_eIDX == "Yes") {
  data_eIDXIDX_merge <-
    data_eIDXIDX_visits[!(data_eIDXIDX_visits$`Sch SchDept` %in% remove_departments_eIDX$`Sch SchDept`), ]

  # Date Range Check
  # eIDX/IDX
  choices_date_range_eIDX <-
    format(
      unique(data_eIDXIDX_visits$`SchDateId Date (MM/DD/YYYY)`),
      "%m/%d/%Y"
    )
  remove_dates_eIDX <-
    select.list(
      choices = c("None", choices_date_range_eIDX),
      title = "eIDX/IDX:Remove Dates?",
      multiple = T,
      graphics = T,
      preselect = "None"
    )
  if (remove_dates_eIDX != "None" | is.na(remove_dates_eIDX)) {
    remove_dates_eIDX <-
      as.Date(remove_dates_eIDX, tryFormats = "%m/%d/%Y")
    data_eIDXIDX_merge <-
      data_eIDXIDX_visits[!(data_eIDXIDX_visits$`SchDateId Date (MM/DD/YYYY)` %in% remove_dates_eIDX), ]
  }
  
  # Selecting only needed columns
  data_eIDXIDX_merge <-
    data_eIDXIDX_merge[, c(
      "Cost Center",
      "Start Date",
      "End Date",
      "VolumeID",
      "Sch Visit Num"
    )]
  colnames(data_eIDXIDX_merge) <-
    c("Cost Center", "Start Date", "End Date", "Volume ID", "Volume")
}

data_Epic_merge <-
  data_Epic[!data_Epic$Department %in% remove_departments_Epic, ]

# remove dates if user chooses
# Epic
choices_date_range_Epic <-
  format(unique(data_Epic$Appt.Date), "%m/%d/%Y")
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
    data_Epic_merge[!(data_Epic_merge$Appt.Date %in% remove_dates_Epic), ]
} # remove dates if user chooses

# Selecting only needed columns
data_Epic_merge <-
  data_Epic_merge[, c("Cost Center", "Start Date", "End Date", "Volume ID")]
data_Epic_merge$Volume <-
  rep(1, length(data_Epic_merge$`Cost Center`))
colnames(data_Epic_merge) <-
  c("Cost Center", "Start Date", "End Date", "Volume ID", "Volume")

# Merging Data
if (answer_eIDX == "Yes") {
  data_visits <- rbind(data_eIDXIDX_merge, data_Epic_merge)
} else {
  data_visits <- data_Epic_merge
}
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
