# Libraries ---------------------------------------------------------------
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(rstudioapi)
library(DBI)
library(odbc)

# Assigning Directory ------------------------------------------------

# rightsourcing project path
project_path <- paste0("/SharedDrive/deans/Presidents/SixSigma/",
                       "MSHS Productivity/Productivity/Labor - Data/",
                       "Rightsourcing Labor/")
# universal mapping path
mapping_path <- paste0("/SharedDrive/deans/Presidents/SixSigma/",
                       "MSHS Productivity/Productivity/universal Data/",
                       "Mapping/")

oao_con <- dbConnect(odbc(), "OAO Cloud DB Production")

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
                 na.strings = c("", "NA"))
  
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
pay_period_mapping <- tbl(oao_con, "LPM_MAPPING_PAYCYCLE") %>% collect()
# modifying column names in order to not have to recode the rest of the script
pay_period_mapping <- pay_period_mapping %>%
  rename(DATE = PAYCYCLE_DATE,
         START.DATE = PP_START_DATE,
         END.DATE = PP_END_DATE,
         PREMIER.DISTRIBUTION = PREMIER_DISTRIBUTION)
# code conversion mapping file to convert legacy to oracle cc
# code_conversion <- read_xlsx(paste0(mapping_path,
#                                     "MSHS_Code_Conversion_Mapping_no_duplicates.xlsx"))
code_conversion <- read_xlsx(paste0(
  "/SharedDrive/deans/Presidents/SixSigma/",
  "Individual Folders/Current Employees/Engineers/Matthew Miesle/LPM/Analysis from Desktop/2024-10-15 Rightsourcing MSMW merge/",
  "MSHS_Code_Conversion_Mapping_no_duplicates - MM check for rightsourcing facility.xlsx"))

# report mapping file for QC check to identify published departments
report_info <- read_xlsx(paste0(mapping_path,
                                "MSHS_Reporting_Definition_Mapping.xlsx"))

# user needs most recent raw data file
raw_data <- recent_file(path = paste0(project_path, "Source Data/MSHS"),
                        file_header = T,
                        encoding = "UTF-16LE",
                        delimeter = "\t",
                        premier = FALSE)

# user needs previous raw data file to compare column headers
raw_data_prev <- recent_file(path = paste0(project_path, "Source Data/MSHS"),
                             file_header = T,
                             encoding = "UTF-16LE",
                             delimeter = "\t",
                             desc_order = 2,
                             premier = FALSE)

## column header check ----------------------------------------------------
new_col <-
  colnames(raw_data)[!(colnames(raw_data) %in% colnames(raw_data_prev))]
new_col <- new_col %>%
  data.frame()
colnames(new_col) <- c("Column")
new_col <- new_col %>%
  mutate(Status = "new")

missing_col <-
  colnames(raw_data_prev)[!(colnames(raw_data_prev) %in% colnames(raw_data))]
missing_col <- missing_col %>%
  data.frame()
colnames(missing_col) <- c("Column")
missing_col <- missing_col %>%
  mutate(Status = "missing")

col_check <- rbind(new_col, missing_col)

if (length(col_check$Column) > 0) {
  col_check_stop <- showQuestion(
    title = "Missing columns",
    message = paste0(
      "There are columns that are new and/or missing.  ",
      "Review the col_check dataframe for details.  ",
      "To stop running this script, press \"Cancel\".  ",
      "If you have already confirmed that the data is ok ",
      "press \"OK\" to continue running the script."
    )
  )
} else {
  col_check_stop <- TRUE
}

if (col_check_stop == FALSE) {
  stop("Script is discontinued by your request.")
}

## remove prev raw data ---------------------------------------------------

rm(raw_data_prev)

## previous upload import -------------------------------------------------

#user needs most recent zero and upload files
msbib_zero_old <- recent_file(path = paste0(project_path, "MSBIB/Zero"),
                              text_cols = rep("character", 14), file_header = T)
msbib_upload_old <- recent_file(path = paste0(project_path, "MSBIB/Uploads"),
                                text_cols = rep("character", 14), file_header = T)
mshq_zero_old <- recent_file(path = paste0(project_path, "MSHQ/Zero"),
                             text_cols = rep("character", 14), file_header = T)
mshq_upload_old <- recent_file(path = paste0(project_path, "MSHQ/Uploads"),
                               text_cols = rep("character", 14), file_header = T)
msmw_zero_old <- recent_file(path = paste0(project_path, "MSMW/Zero"),
                           text_cols = rep("character", 14), file_header = T)
msmw_upload_old <- recent_file(path = paste0(project_path, "MSMW/Uploads"),
                              text_cols = rep("character", 14), file_header = T)
# Constants ------------------------------------------------------

# user needs to select the site(s) they want to process rightsourcing for
sites <- select.list(
  choices = c("MSHS", "MSBIB", "MSHQ", "MSMW"),
  title = "Select Output Site(s)",
  graphics = T,
  preselect = "MSHS"
)

#Table of distribution dates earlier than the current date
dist_dates <- pay_period_mapping %>%
  select(END.DATE, PREMIER.DISTRIBUTION) %>%
  distinct() %>%
  drop_na() %>%
  arrange(END.DATE) %>%
  filter(PREMIER.DISTRIBUTION %in% c(TRUE, 1),
         END.DATE < as.POSIXct(Sys.Date()))

#Selecting the most recent distribution date
distribution_date <- dist_dates$END.DATE[nrow(dist_dates)]

#Confirming distribution date which will be the max of the current upload
answer <- showQuestion(
  title = "Distribution Date Confirmation",
  message = paste0(
    "Current distribution will be ", distribution_date, ".  ",
    "If this is correct, press OK.  ",
    "If this is not correct, press Cancel and ",
    "you will be prompted to select the correct ",
    "distribution date."
  )
)

if (answer == FALSE) {
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

prev_0_max_date_msmw <- max(mdy(msmw_zero_old$date.end))

# need threshold for weekly hour total for an employee to flag for review
week_reg_hr_indiv_emp_qc <- 40
week_hr_indiv_emp_qc <- 55

# constant for name of employee that needs to be removed from data
employee_removal <- "Vistharla, Moses"

# regular rate threshold for exempt employees
exempt_payrate <- 1000

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

# create zero upload for MSMW
msmw_zero_new <- msmw_upload_old %>%
  filter(mdy(date.start) > prev_0_max_date_msmw) %>%
  mutate(hours = "0",
         spend = "0")
## New Upload Preprocessing --------------------------------------------------

# apply employee removal filter
processed_data <- raw_data %>%
  filter(!Worker.Name %in% employee_removal) %>%
  mutate(Worker.Name = gsub("\'", "", Worker.Name),
         Worker.Name = gsub("\\(Mt Sinai\\)", "", Worker.Name),
         Worker.Name = gsub(" ,", ",", Worker.Name),
         Worker.Name = iconv(Worker.Name, from = 'UTF-8', to = 'ASCII//TRANSLIT'))

# filter raw data on date range needed for upload
processed_data <- processed_data %>%
  filter(mdy(Earnings.E.D) > min(c(prev_0_max_date_mshq,
                                   prev_0_max_date_msbib,
                                   prev_0_max_date_msmw)),
         mdy(Earnings.E.D) <= distribution_date)

# process department.billed to get oracle home and legacy worked department
processed_data <- processed_data %>%
  filter(Department.Billed != "") %>%
  mutate(cost_center_info =
           str_sub(Department.Billed, nchar("Department:") + 1, -1)) %>%
  mutate(cost_center_info =
           str_sub(cost_center_info, 1,
                   str_locate(cost_center_info, "\\*")[, 1] - 1)) %>%
  mutate(wrkd_dept_leg = case_when(
    nchar(cost_center_info) == 12 ~ substr(cost_center_info, 1, 8),
    nchar(cost_center_info) == 30 ~ str_c(substr(cost_center_info, 1, 4),
                                          substr(cost_center_info, 13, 14),
                                          substr(cost_center_info, 16, 19)),
    TRUE ~ cost_center_info)
  )
  # ) %>%
#   mutate(home_dept_oracle = case_when(
#     substr(wrkd_dept_leg, 1, 4) == "0130" ~ "101010101010102",
#     substr(wrkd_dept_leg, 1, 4) == "4709" ~ "900000040790000",
#     Facility == "MSSL- Mount Sinai St. Lukes" ~ "302020202020202",
#     Facility == "MSW - Mount Sinai West" ~ "301010101010101",
#     nchar(cost_center_info) == 12 ~ "101010101010101",
#     nchar(cost_center_info) == 30 ~ "900000040490000",
#     TRUE ~ cost_center_info
#   )) %>%
#   mutate(hospital = case_when(
#     Facility == "MSSL- Mount Sinai St. Lukes" ~ "NY2163",
#     Facility == "MSW - Mount Sinai West" ~ "NY2162",
#     nchar(cost_center_info) == 12 ~ "NY0014",
#     nchar(cost_center_info) == 30 ~ "630571",
#     TRUE ~ cost_center_info
#   )) 

# join to get oracle departments
row_count <- nrow(processed_data)
processed_data <- processed_data %>%
  left_join(select(code_conversion, COST.CENTER.LEGACY, COST.CENTER.ORACLE,
                   Rightsourcing.Facility, Rightsourcing.Home),
            by = c("wrkd_dept_leg" = "COST.CENTER.LEGACY")) %>%
  mutate(home_dept_oracle = case_when(
    !is.na(Rightsourcing.Home) ~ as.character(Rightsourcing.Home),
    substr(wrkd_dept_leg, 1, 4) == "0130" ~ "101010101010102",
    substr(wrkd_dept_leg, 1, 4) == "4709" ~ "900000040790000",
    substr(wrkd_dept_leg, 1, 6) == "110902" ~ "302020202020202",
    substr(wrkd_dept_leg, 1, 4) == "1109" ~ "301010101010101",
    nchar(cost_center_info) == 12 ~ "101010101010101",
    nchar(cost_center_info) == 30 ~ "900000040490000",
    TRUE ~ cost_center_info
  )) %>%
  mutate(hospital = case_when(
    !is.na(Rightsourcing.Facility) ~ Rightsourcing.Facility,
    substr(wrkd_dept_leg, 1, 4) == "0130" ~ "NY0014",
    substr(wrkd_dept_leg, 1, 4) == "4709" ~ "630571",
    substr(wrkd_dept_leg, 1, 6) == "110902" ~ "NY2163",
    substr(wrkd_dept_leg, 1, 4) == "1109" ~ "NY2162",
    nchar(cost_center_info) == 12 ~ "NY0014",
    nchar(cost_center_info) == 30 ~ "630571",
    TRUE ~ cost_center_info
  )) %>%
  mutate(wrkd_dept_oracle = case_when(
    is.na(COST.CENTER.ORACLE) ~ home_dept_oracle,
    TRUE ~ COST.CENTER.ORACLE
  ))

# quality check left join to make sure row count has not changed
if (row_count != nrow(processed_data)) {
  showDialog(
    title = "Join Error",
    message = paste0("Error in code conversion mapping.  ",
                     "Row count has been changed by left join")
  )
  stop(paste0("Error in code conversion mapping.  ",
              "Row count has been changed by left join"))
}

# get list of legacy cost centers that are not in code conversion
cc_map_fail <- processed_data %>%
  select(wrkd_dept_leg, Department.Billed) %>%
  filter(!(wrkd_dept_leg %in% code_conversion$COST.CENTER.LEGACY)) %>%
  distinct() %>%
  mutate(Department.Billed =
           str_sub(Department.Billed,
                   str_locate(Department.Billed, "\\*")[, 1] + 1, -1))

View(cc_map_fail)

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
  mutate(jobcode = str_pad(jobcode, 5, side = "left", pad = "0")) %>%
  mutate(jobcode = paste0("R", jobcode))

jobcode_list_new <- rbind(jobcode_list, jc_new)

# join existing job codes
row_count <- nrow(processed_data)
processed_data <- processed_data %>%
  left_join(jobcode_list_new)

# quality check left join to make sure row count has not changed

if (row_count != nrow(processed_data)) {
  showDialog(
    title = "Join Error",
    message = paste0("Error in job code mapping.  ",
                     "Row count has been changed by left join")
  )
  stop(paste0("Error in job code mapping.  ",
              "Row count has been changed by left join"))
}

# jc dictionary upload for all combinations in the latest raw data file
jc_dict_upload <- processed_data %>%
  select(hospital, wrkd_dept_oracle, jobcode, Job.Title) %>%
  mutate(system = "729805") %>%
  relocate(system, .before = hospital) %>%
  distinct() %>%
  mutate(Job.Title = substr(Job.Title, 1, 50))

## Summarizing Hours and Expenses-------------------------------------------

# all daily hours need to be summed up
processed_data <- processed_data %>%
  rowwise() %>%
  mutate(daily_hours =
           case_when(Bill.Type == "Time" ~ sum(Regular.Hours, OT.Hours, 
                                               Holiday.Hours,
                                               na.rm = T),
                     Bill.Type == "Adjustment" ~ Weekly.Hours))

# Day Spend needs to be in numerical decimal format to summarize it
processed_data <- processed_data %>%
  mutate(Regular.Rate = 
           as.numeric(str_trim(gsub("[$,]", "", Regular.Rate)))) %>%
  mutate(
    Day.Spend.char = Day.Spend,
    Day.Spend = 
      case_when(Bill.Type == "Adjustment" | Regular.Rate > exempt_payrate ~
                  as.numeric(str_trim(gsub("[$,]", "", Time.Card.Spend))),
                Bill.Type == "Time" ~ 
                  as.numeric(str_trim(gsub("[$,]", "", Day.Spend)))))

# special handling for exempt employee Time and 0 hour Adjustment 
processed_data <- processed_data %>%
  mutate(daily_hours = 
           case_when(Regular.Rate > exempt_payrate ~ 
                       round(40 * Day.Spend/Regular.Rate, digits = 2),
                     daily_hours == 0 & Bill.Type == "Adjustment" ~ 
                       round(Day.Spend/Regular.Rate, digits = 2),
                     TRUE ~ daily_hours))


# need to summarize data
rolled_up <- processed_data %>%
  group_by(hospital, home_dept_oracle,  wrkd_dept_oracle, Earnings.E.D,
           Worker.Name, jobcode) %>%
  summarize(across(c(daily_hours, Day.Spend), \(x) sum(x, na.rm = T))) %>%
  ungroup() %>%
  rename(week_hours = daily_hours,
         week_spend = Day.Spend)


# Data Formatting ---------------------------------------------------------

# upload needs to be formatted for premier upload criteria
upload_new <- rolled_up %>%
  mutate(partner = "729805", .before = hospital) %>%
  mutate(hospital_worked = hospital, .before = wrkd_dept_oracle) %>%
  mutate(start_date = format(mdy(Earnings.E.D) - 6, format = "%m/%d/%Y"),
         .before = Earnings.E.D) %>%
  mutate(employee_id = paste0(
    substr(trimws(sub(",.*", "", Worker.Name)), 1, 6),
    substr(gsub("\\..*", "", week_spend), 1, 3),
    substr(wrkd_dept_oracle, 13, 15),
    substr(jobcode, 4, 6)),
    .before = Worker.Name) %>%
  mutate(Worker.Name = substr(Worker.Name, 1, 30)) %>%
  mutate(approved_hours = "0", .before = jobcode) %>%
  mutate(paycode = "AG1", .before = week_hours) %>%
  mutate(week_hours = round(week_hours, digits = 2),
         week_spend = round(week_spend, digits = 2))

# Quality Checks ----------------------------------------------------------

## Trend hours ------------------------------------------------------------

# trend hours by cost center by Earnings.E.D to check hours trend
qc_hours_by_cc <- upload_new %>%
  group_by(wrkd_dept_oracle, Earnings.E.D) %>%
  summarise(Hours = sum(week_hours, na.rm = T)) %>%
  arrange(desc(Hours)) %>%
  arrange(mdy(Earnings.E.D)) %>%
  left_join(distinct(select(code_conversion,
                            COST.CENTER.ORACLE,
                            COST.CENTER.DESCRIPTION.ORACLE)),
            by = c("wrkd_dept_oracle" = "COST.CENTER.ORACLE")) %>%
  pivot_wider(id_cols = c(wrkd_dept_oracle, COST.CENTER.DESCRIPTION.ORACLE),
              names_from = Earnings.E.D,
              values_from = Hours)

View(qc_hours_by_cc)

## Employee check ---------------------------------------------------------

### Regular hours ---------------------------------------------------------

# get total regular hours in each week by employee and above regular hours
# threshold
hrs_reg_indiv_emp <- processed_data %>%
  group_by(Worker.Name, Earnings.E.D) %>%
  summarize(week_hours_reg = sum(Regular.Hours, na.rm = T)) %>%
  ungroup() %>%
  filter(week_hours_reg > week_reg_hr_indiv_emp_qc) %>%
  arrange(-week_hours_reg, Worker.Name, as.Date(Earnings.E.D, "%m/%d/%Y"))

View(hrs_reg_indiv_emp)

# filter process_data down to the employees with high reg hours that are in
# Premier reports
high_hr_reg_emp <- processed_data %>%
  left_join(
    filter(
      select(report_info, DEFINITION.CODE, DEFINITION.NAME,
             ORACLE.COST.CENTER, DEPARTMENT.BREAKDOWN, CLOSED),
      is.na(CLOSED)),
    c("wrkd_dept_oracle" = "ORACLE.COST.CENTER")) %>%
  inner_join(hrs_reg_indiv_emp) %>%
  filter(DEPARTMENT.BREAKDOWN == 1) %>%
  arrange(-week_hours_reg, Worker.Name, as.Date(Earnings.E.D, "%m/%d/%Y"),
          as.Date(Date.Worked, "%m/%d/%Y"))

View(high_hr_reg_emp)

### Total hours ---------------------------------------------------------

# get total hours in each week by employee and filter above threshold
hrs_indiv_emp <- processed_data %>%
  group_by(Worker.Name, Earnings.E.D) %>%
  summarize(week_hours = sum(daily_hours, na.rm = T)) %>%
  ungroup() %>%
  filter(week_hours > week_hr_indiv_emp_qc) %>%
  arrange(-week_hours, Worker.Name, as.Date(Earnings.E.D, "%m/%d/%Y"))

View(hrs_indiv_emp)

# filter process_data down to the employees with high total hours that are in
# Premier reports
high_hr_emp <- processed_data %>%
  left_join(
    filter(
      select(report_info, DEFINITION.CODE, DEFINITION.NAME,
             ORACLE.COST.CENTER, DEPARTMENT.BREAKDOWN, CLOSED),
      is.na(CLOSED)),
    c("wrkd_dept_oracle" = "ORACLE.COST.CENTER")) %>%
  filter(DEPARTMENT.BREAKDOWN == 1) %>%
  inner_join(hrs_indiv_emp) %>%
  arrange(-week_hours, Worker.Name, as.Date(Earnings.E.D, "%m/%d/%Y"),
          as.Date(Date.Worked, "%m/%d/%Y"))

View(high_hr_emp)

# qc for employees with regular payrate > $1000
high_payrate <- processed_data %>%
  filter(Regular.Rate > exempt_payrate)

View(high_payrate)


# File Saving -------------------------------------------------------------

## Premier 2.0 output format updates --------------------------------------

### Jobcode Dictionary Format ---------------------------------------------

jc_dict_upload <- jc_dict_upload %>%
  mutate(wrkd_dept_oracle = NULL) %>%
  distinct() %>%
  mutate(dahr = NA,
         esd = NA,
         exp_date = NA)

jc_dict_upload_cols <- c("Corporation Code",
                         "Entity Code",
                         "Job Code",
                         "Job Code Name",
                         "Default Agency Hourly Rate",
                         "Effective Start Date",
                         "Expiration Date")

colnames(jc_dict_upload) <- jc_dict_upload_cols

### Upload Format ---------------------------------------------------------

upload_payroll_cols <- c("Corporation Code",
                         "Home Entity Code",
                         "Home Cost Center Code",
                         "Worked Entity Code",
                         "Worked Cost Center Code",
                         "Start Date",
                         "End Date",
                         "Employee Code",
                         "Employee Name",
                         "Approved Hours per Pay Period",
                         "Job Code",
                         "Pay Code",
                         "Hours",
                         "Expense")

colnames(upload_new) <- upload_payroll_cols
colnames(mshq_zero_new) <- upload_payroll_cols
colnames(msbib_zero_new) <- upload_payroll_cols
colnames(msmw_zero_new) <- upload_payroll_cols
## Upload Files -----------------------------------------------------------

if (sites == "MSHS" | sites == "MSHQ") {
  # save MSHQ upload
  write.table(filter(upload_new, `Worked Entity Code` == "NY0014"),
              paste0(project_path,
                     "MSHQ/Uploads/MSHQ_Rightsourcing_",
                     min(mdy(upload_new$`Start Date`)), "_",
                     max(mdy(upload_new$`End Date`)), "_MM_test.csv"),
              row.names = F, col.names = T, sep = ",")
  
  # save MSHQ zero file
  write.table(mshq_zero_new, paste0(project_path,
                                    "MSHQ/Zero/MSHQ_Rightsourcing Zero_",
                                    min(mdy(mshq_zero_new$`Start Date`)), "_",
                                    max(mdy(mshq_zero_new$`End Date`)), "_MM_test.csv"),
              row.names = F, col.names = T, sep = ",")
}

if (sites == "MSHS" | sites == "MSBIB") {
  # save MSBIB upload
  write.table(filter(upload_new, `Worked Entity Code` == "630571"),
              paste0(project_path,
                     "MSBIB/Uploads/MSBIB_Rightsourcing_",
                     min(mdy(upload_new$`Start Date`)), "_",
                     max(mdy(upload_new$`End Date`)), "_MM_test.csv"),
              row.names = F, col.names = T, sep = ",")
  
  # save MSBIB zero file
  write.table(msbib_zero_new, paste0(project_path,
                                     "MSBIB/Zero/MSBIB_Rightsourcing Zero_)",
                                     min(mdy(msbib_zero_new$`Start Date`)), "_",
                                     max(mdy(msbib_zero_new$`End Date`)), "_MM_test.csv"),
              row.names = F, col.names = T, sep = ",")
}

if (sites == "MSHS" | sites == "MSMW") {
  # Combine MSM and MSW upload files
  msmw_upload <- rbind(
    filter(upload_new, `Worked Entity Code` == "NY2163"),  # MSM upload
    filter(upload_new, `Worked Entity Code` == "NY2162")   # MSW upload
  )
  
  # Save combined upload file
  write.table(msmw_upload,
              paste0(project_path,
                     "MSMW/Uploads/MSMW_Rightsourcing_",
                     min(mdy(msmw_upload$`Start Date`)), "_",
                     max(mdy(msmw_upload$`End Date`)), "_MM_test.csv"),
              row.names = F, col.names = T, sep = ",")
  
  # Save combined zero file
  write.table(msmw_zero_new,
              paste0(project_path,
                     "MSMW/Zero/MSMW_Rightsourcing Zero_",
                     min(mdy(msmw_zero_new$`Start Date`)), "_",
                     max(mdy(msmw_zero_new$`End Date`)), "_MM_test.csv"),
              row.names = F, col.names = T, sep = ",")
}

## Jobcode Items -----------------------------------------------------------

# overwrite job code list
write.table(jobcode_list_new, paste0(project_path, "Rightsource Job Code_MM_test.csv"),
            row.names = F, sep = ",")

# save jobcode dictionary upload
write.table(jc_dict_upload, paste0(project_path, "Jobcode Dictionary/",
                                   "MSHS_Jobcode Dictionary_",
                                   max(mdy(upload_new$`End Date`)),
                                   "_MM_test.csv"),
            row.names = F, col.names = T, sep = ",", na = "")

# save unmapped cost center list
write.table(cc_map_fail, paste0(project_path, "Failed Cost Center Mappings/",
                                "MSHS_Failed Cost Center Mappings_",
                                max(mdy(upload_new$`End Date`)),
                                "_MM_test.csv"),
            row.names = F, sep = ",")

# Script End --------------------------------------------------------------
