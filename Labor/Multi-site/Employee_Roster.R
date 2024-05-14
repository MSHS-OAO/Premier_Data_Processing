# Libraries ------------------------------------------------------------------
library(tidyverse)
library(DBI)
library(odbc)
library(openxlsx)

# Directories ---------------------------------------------------------------
employee_roster_dir <- paste0("/SharedDrive/deans/Presidents/SixSigma/",
                              "MSHS Productivity/Productivity/Analysis/",
                              "Employee Roster/")

# Constants -----------------------------------------------------------------
# oao_production db connection
oao_con <- dbConnect(odbc(), "OAO Cloud DB Production")

## User Inputs --------------------------------------------------------------
# date range
start_date <- "2024-01-01"
end_date <- "2024-03-23"
# user inputs the department and respective cost center(s)
# do not include Site in the department name
### Example -----------------------------------------------------------------
# departments <- list(
#   OBGYN = c("101000010113121", "101000010113122"),
#   PEDS = c("101000010110260", "101000010113303", "101000010113304"),
#   IMA = c("101000010112815", "101000010112816")
# )

departments <- list(
  LIVER = c("101000010112818", "101000010112817"),
  GERIATRICS = c("101000010113821", "101000010112790"),
  RHEUMATOLOGY = c("101000010112838", "101000010112839"),
  RESPIRATORY = c("101000010121153", "101000010121152"),
  OPTHALMOLOGY = c("101000010113155", "101000010113153")
)

# Roster Creation -----------------------------------------------------------
employee_roster <- lapply(departments, function(x) {
  # get data for necessary cost centers and aggregate
  roster <- tbl(oao_con, "LPM_MAPPED_MSHS_ORACLE") %>%
    filter(WORKED_DEPARTMENT %in% x,
           PP_END_DATE >= as.Date(start_date),
           PP_END_DATE <= as.Date(end_date),
           WORKED_PAYCODE == 1,
           PROVIDER == 0,
           INCLUDE_HOURS == 1) %>%
    group_by(WORKED_DEPARTMENT, WORKED_DEPARTMENT_NAME, EMPLOYEE_ID, 
           EMPLOYEE_NAME, POSITION_CODE_DESCRIPTION, PP_END_DATE) %>%
    summarise(FTE = sum(WD_HOURS, na.rm = TRUE)) %>%
    arrange(PP_END_DATE) %>%
    collect()
  
  # pivot data for roster format
  roster <- roster %>%
    mutate(FTE = format(round(FTE, 2), nsmall = 2),
           FTE = as.character(FTE)) %>%
    pivot_wider(id_cols = c(WORKED_DEPARTMENT, WORKED_DEPARTMENT_NAME,
                            EMPLOYEE_ID, EMPLOYEE_NAME,
                            POSITION_CODE_DESCRIPTION),
                names_from = PP_END_DATE,
                values_from = FTE) %>%
    arrange(WORKED_DEPARTMENT)
  
  return(roster)
})

# Save Rosters --------------------------------------------------------------
# create workbook
wb <- createWorkbook()

# create sheet for each roster and apply styling
for (i in 1:length(employee_roster)) {
  # create sheet name
  sheet <- addWorksheet(wb, names(employee_roster)[i])
  
  # set freeze pane and col widths
  freezePane(wb, sheet, firstActiveRow = 2)
  setColWidths(wb, sheet, cols = 1:ncol(employee_roster[[i]]), widths = "auto")
  
  # write data to sheet
  writeDataTable(wb,
                 sheet = sheet,
                 x = as.data.frame(employee_roster[[i]]),
                 keepNA = FALSE,
                 withFilter = TRUE)
  
  #create cell styles
  header_style <- createStyle(fgFill = "grey", halign = "left", 
                             textDecoration = "bold", fontColour = "#010101")
  body_style <- createStyle(border = "TopBottomLeftRight")
  
  # apply cell styles
  addStyle(wb, sheet, header_style, rows = 1, 
           cols = 1:ncol(employee_roster[[i]]), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, body_style, rows = 1:(nrow(employee_roster[[i]]) + 1),
           cols = 1:ncol(employee_roster[[i]]), gridExpand = TRUE, stack = TRUE)
  
  
}

# save rosters as workbook with system time
saveWorkbook(wb, file = paste0(employee_roster_dir, "Employee_Roster_",
                               Sys.time(), ".xlsx"))
