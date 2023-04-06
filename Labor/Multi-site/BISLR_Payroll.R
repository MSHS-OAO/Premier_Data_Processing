
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(xlsx)
library(rstudioapi)
library(stringr)
library(stringi)

# clear memory of all objects including those hidden
rm(list = ls(all.names = TRUE))

# Directories -------------------------------------------------------------
dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity"
dir_BISLR <- paste0(dir, "/Labor - Data/Multi-site/BISLR")
dir_universal <- paste0(dir, "/Universal Data")

# User Warnings -----------------------------------------------------------

showDialog(
  title = "Updated Premier Dictionary Mapping Files",
  message = paste0(
    "USER WARNING: ",
    "Before proceeding, ",
    "be sure that all the latest Premier Dictionary and Mapping Exports ",
    "have been saved in the folder listed below.  ",
    "    Issues can arise if the files are not the latest version.     ",
    dir, "/Universal Data/Premier")
  )

showDialog(
  title = "Script Improvement",
  message = paste0(
    "Please update the script to ensure Cost.Center.Description values ",
    "are not blank in the fte_summary quality check output."
    )
)

# Constants ---------------------------------------------------------------
new_dpt_map <- 10095
map_effective_date_dpt <- as.Date("2010-01-01")
corp_code <- 729805
jc_alloc_pct <- 100

accrual_report_ids <- c("DNU_8600", "DNU_MSM_8600", "DNU_MSW_8600")
true_accrual_cc_desc <- c("ACCRUAL COST CENTER", "SPECIAL FUNDS ACCOUNTING",
                          "GENERAL ACCOUNTING", "FPA BIMG")

dummy_report_ids <- c("DNU_000", "DNU_MSM000", "DNU_MSW000")

jc_desc_threshold <- 5

## Premier Formatting ------------------------------------------------------
char_len_dpt <- 15
char_len_dpt_name <- 50
char_len_employ <- 30
char_len_jobcode <- 10
char_len_paycode <- 15
char_len_paycode_name <- 50

# Functions --------------------------------------------------------------
import_recent_file <- function(folder.path, place) {
  # Importing File information from Folder
  File.Name <- list.files(path = folder.path, full.names = F)
  File.Path <- list.files(path = folder.path, full.names = T)
  File.Date <- as.Date(sapply(File.Name,
                              function(x) {
                                if (substr(x, nchar(x) - 3,
                                           nchar(x)) == ".txt") {
                                  substr(x, nchar(x) - 18, nchar(x) - 9)
                                } else {
                                  (substr(x, nchar(x) - 14, nchar(x) - 5))
                                }
                              }),
                       format = "%m_%d_%Y")
  File.Table <<- data.table::data.table(File.Name, File.Date, File.Path) %>%
    arrange(desc(File.Date))
  # Quality Check - Confirming Upload File
  answer <- showQuestion(title = "Question",
                         message = paste0("File selected is: \n",
                                          File.Table$File.Name[place], "\n",
                                          "Is this the correct file?"),
                         ok = "Yes", cancel = "No")
  if (answer == F) {
    user_selected_file <- select.list(choices = File.Table$File.Name,
                                      multiple = F,
                                      title = "Select the correct file",
                                      graphics = T)
    place <- grep(user_selected_file, File.Table$File.Name)
    cat("File selected is ", File.Table$File.Name[place])
  }
  # Importing Data
  data_recent <- read.csv(File.Table$File.Path[place],
                          header = T, sep = "~", fill = T)
  return(data_recent)
}

# Import Data -------------------------------------------------------------
raw_payroll <- import_recent_file(paste0(dir_BISLR, "/Source Data"), 1)

# Import References -------------------------------------------------------
pay_cycles_uploaded <- read.xlsx(paste0(dir_BISLR, "/Reference",
                                        "/Pay cycles uploaded_Tracker.xlsx"),
                                 sheetIndex = 1)
msus_removal_list <- read_xlsx(paste0(dir_BISLR,
                                      "/Reference/MSUS_removal_list.xlsx"),
                               sheet = 1)
## Universal Reference Files -----------------------------------------------
map_uni_paycodes <- read_xlsx(paste0(dir_universal,
                                     "/Mapping/MSHS_Paycode_Mapping.xlsx"),
                              sheet = 1)
map_uni_jobcodes <- read_xlsx(paste0(dir_universal,
                                     "/Mapping/MSHS_Jobcode_Mapping.xlsx"),
                              sheet = 1)
map_uni_reports <- read_xlsx(paste0(dir_universal,
                                    "/Mapping",
                                    "/MSHS_Reporting_Definition_Mapping.xlsx"),
                             sheet = 1)
map_uni_paycycles <- read_xlsx(paste0(dir_universal,
                                      "/Mapping/MSHS_Pay_Cycle.xlsx"),
                               sheet = 1)

## Premier Reference Files -------------------------------------------------
dict_premier_dpt <- read.csv(paste0(dir_universal,
                                    "/Premier/Dictionary Exports",
                                    "/DepartmentDictionary.csv"),
                             col.names = c("Corporation.Code", "Site",
                                           "Cost.Center",
                                           "Cost.Center.Description"),
                             sep = ",")
map_premier_dpt <- read.csv(paste0(dir_universal, "/Premier/Mapping Exports",
                                   "/DepartmentMapping.csv"),
                            col.names = c("Effective.Date", "Corporation.Code",
                                          "Site", "Cost.Center",
                                          "Cost.Center.Map"),
                            sep = ",")
dict_premier_jobcode <- read.csv(paste0(dir_universal,
                                        "/Premier/Dictionary Exports",
                                        "/DeptJobCodeDictionary.csv"),
                                 col.names = c("Corporation.Code", "Site",
                                               "Cost.Center", "Job.Code",
                                               "Job.Code.Description"),
                                 sep = ",")
dict_premier_report <- read.csv(paste0(dir_universal,
                                       "/Premier/Dictionary Exports",
                                       "/DepartmentDef.csv"),
                                col.names = c("Corporation.Code", "Site",
                                              "Report.Name", "Report.ID",
                                              "Cost.Center", "Report.Type",
                                              "Threshold.Type", "Target.Type",
                                              "Exclude.Report.Rollup",
                                              "Effective.Date", "Paycycle.Type",
                                              "Exclude.Admin.Rollup",
                                              "Exclude.Action.Plan",
                                              "blank14", "blank15",
                                              "blank16", "blank17"),
                                sep = ",", fill = T)

dict_premier_paycode <- read.csv(paste0(dir_universal,
                                        "/Premier/Dictionary Exports",
                                        "/PayCodeDictionaryExport.csv"),
                                 header = TRUE,
                                 sep = ",")


## Quality - piv_wide_check ------------------------------------------------

piv_wide_check_prev <- read.csv(paste0(dir_BISLR, "/Quality Checks",
                                       "/piv_wide_check", ".csv"))

# Preprocessing --------------------------------------------------------------


## References --------------------------------------------------------------
map_uni_jobcodes <- map_uni_jobcodes %>%
  mutate(J.C = str_trim(J.C)) %>%
  mutate(JC_in_UniversalFile = 1)
map_uni_paycodes <- map_uni_paycodes %>%
  mutate(Paycode_in_Universal = 1)
pay_cycles_uploaded <- pay_cycles_uploaded %>%
  select(-capture_time) %>%
  mutate(Pay_Cycle_Uploaded = 1)
dict_premier_dpt <- dict_premier_dpt %>%
  mutate(Cost.Center = as.character(Cost.Center), Dpt_in_Dict = 1)
dict_premier_jobcode <- dict_premier_jobcode %>%
  mutate(JC_in_Dict = 1)

reports_dept <- str_split(dict_premier_report$Cost.Center,
  pattern = ":", simplify = T) %>%
  as.data.frame()
report_list <- lapply(
  seq_len(nrow(reports_dept)),
  function(x) {
    pivot_longer(reports_dept[x, ],
                 cols = everything())
  }
)
report_list <- lapply(seq_len(length(report_list)),
                      function(x) subset(report_list[[x]], value != ""))
report_list <- lapply(
  seq_len(length(report_list)),
  function(x) {
    mutate(report_list[[x]],
      Site = dict_premier_report$Site[x],
      Report.ID = dict_premier_report$Report.ID[x],
      Effective.Date = dict_premier_report$Effective.Date[x])
  }
)
report_list <- do.call(rbind, report_list) %>%
  rename(Cost.Center = value) %>%
  select(-name)

dist_dates <- map_uni_paycycles %>%
  select(END.DATE, PREMIER.DISTRIBUTION) %>%
  distinct() %>%
  drop_na() %>%
  arrange(END.DATE) %>%
  filter(PREMIER.DISTRIBUTION %in% c(TRUE, 1),
         END.DATE < max(as.POSIXct(raw_payroll$End.Date, format = "%m/%d/%Y")))

# Selecting the most recent distribution date
distribution_date <- max(as.POSIXct(dist_dates$END.DATE))

dist_prev <- dist_dates$END.DATE[
  which(dist_dates$END.DATE == distribution_date) - 1
]

## Site Hours Quality Check ------------------------------------------------
piv_wide_check <- raw_payroll %>%
  filter(as.Date(End.Date, "%m/%d/%Y") >= dist_prev &
           as.Date(End.Date, "%m/%d/%Y") <= distribution_date +
                                            lubridate::days(7)) %>%
  group_by(Facility.Hospital.Id_Worked, Payroll.Name, End.Date) %>%
  summarize(Hours = sum(as.numeric(Hours), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(as.Date(End.Date, "%m/%d/%Y"),
          Facility.Hospital.Id_Worked, Payroll.Name) %>%
  bind_rows(summarize(group_by(., Facility.Hospital.Id_Worked, End.Date),
                      Hours = sum(Hours, na.rm = TRUE),
                      Payroll.Name = "-SITE TOTAL-")) %>%
  bind_rows(summarize(group_by(filter(., Payroll.Name == "-SITE TOTAL-"),
                               End.Date),
                      Hours = sum(Hours, na.rm = TRUE),
                      across(where(is.character), ~"TOTAL"))) %>%
  arrange(Facility.Hospital.Id_Worked, Payroll.Name,
          as.Date(End.Date, "%m/%d/%Y")) %>%
  mutate(Hours = prettyNum(Hours, big.mark = ",")) %>%
  pivot_wider(names_from = End.Date, values_from = Hours)

View(piv_wide_check)
View(piv_wide_check_prev)


## Data  --------------------------------------------------------------------
row_count <- nrow(raw_payroll)

bislr_payroll <- raw_payroll %>%
  mutate(DPT.WRKD = paste0(substr(Full.COA.for.Worked, 1, 3),
                           substr(Full.COA.for.Worked, 41, 44),
                           substr(Full.COA.for.Worked, 5, 7),
                           substr(Full.COA.for.Worked, 12, 16)),
         DPT.HOME = paste0(substr(Full.COA.for.Home, 1, 3),
                           substr(Full.COA.for.Home, 41, 44),
                           substr(Full.COA.for.Home, 5, 7),
                           substr(Full.COA.for.Home, 12, 16)),
         DPT.WRKD.LEGACY = paste0(substr(Reverse.Map.for.Worked, 1, 4),
                                  substr(Reverse.Map.for.Worked, 13, 14),
                                  substr(Reverse.Map.for.Worked, 16, 19)),
         DPT.HOME.LEGACY = paste0(substr(Reverse.Map.for.Home, 1, 4),
                                  substr(Reverse.Map.for.Home, 13, 14),
                                  substr(Reverse.Map.for.Home, 16, 19)),
         Start.Date = as.Date(Start.Date, format = "%m/%d/%Y"),
         End.Date = as.Date(End.Date, format = "%m/%d/%Y"),
         Employee.Name = substr(Employee.Name, 1, 30),
         Approved.Hours.per.Pay.Period = round(Approved.Hours.per.Pay.Period,
                                               digits = 0),
         Job.Code = str_trim(Job.Code),
         Position.Code.Description = str_trim(Position.Code.Description)) %>%
  mutate(
    DPT.WRKD = case_when(
      trimws(Department.Name.Worked.Dept) == "" ~
        as.character(Department.IdWHERE.Worked),
      TRUE ~ DPT.WRKD),
    DPT.HOME = case_when(
      trimws(Department.Name.Home.Dept) == "" ~
        as.character(Department.ID.Home.Department),
      TRUE ~ DPT.HOME)) %>%
  mutate(
    Job.Code = case_when(
      paste0(DPT.WRKD, "-", toupper(Employee.Name)) %in%
        paste0(msus_removal_list$`Department IdWHERE Worked`, "-",
               msus_removal_list$`Employee Name`) ~
        unique(msus_removal_list$`New Job Code`),
      TRUE ~ Job.Code),
    Position.Code.Description = case_when(
      paste0(DPT.WRKD, "-", toupper(Employee.Name)) %in%
        paste0(msus_removal_list$`Department IdWHERE Worked`, "-",
               msus_removal_list$`Employee Name`) ~
        unique(msus_removal_list$`New Job Code Description`),
      TRUE ~ Position.Code.Description)) %>%
  left_join(pay_cycles_uploaded) %>%
  left_join(map_uni_jobcodes %>%
              filter(PAYROLL == "BISLR") %>%
              select(J.C, PROVIDER, JC_in_UniversalFile) %>%
              rename(Job.Code = J.C)) %>%
  left_join(map_uni_paycodes %>%
              select(RAW.PAY.CODE, Paycode_in_Universal) %>%
              rename(Pay.Code = RAW.PAY.CODE)) %>%
  left_join(dict_premier_dpt %>%
              select(Site, Cost.Center, Dpt_in_Dict) %>%
              rename(Home.FacilityOR.Hospital.ID = Site,
                     DPT.HOME = Cost.Center,
                     HomeDpt_in_Dict = Dpt_in_Dict)) %>%
  left_join(dict_premier_dpt %>%
              select(Site, Cost.Center, Dpt_in_Dict) %>%
              rename(Facility.Hospital.Id_Worked = Site,
                     DPT.WRKD = Cost.Center,
                     WRKDpt_in_Dict = Dpt_in_Dict)) %>%
  left_join(dict_premier_jobcode %>%
              select(Site, Cost.Center, Job.Code, JC_in_Dict) %>%
              rename(Home.FacilityOR.Hospital.ID = Site,
                     DPT.HOME = Cost.Center,
                     HOMEJC_in_Dict = JC_in_Dict)) %>%
  left_join(dict_premier_jobcode %>%
              select(Site, Cost.Center, Job.Code, JC_in_Dict) %>%
              rename(Facility.Hospital.Id_Worked = Site,
                     DPT.WRKD = Cost.Center,
                     WRKJC_in_Dict = JC_in_Dict)) %>%
  mutate(Job.Code_up = substr(Job.Code, 1, 10),
         Approved.Hours.per.Pay.Period = case_when(
           is.na(Approved.Hours.per.Pay.Period) ~ 0,
           TRUE ~ Approved.Hours.per.Pay.Period))

accrual_raw_detail <- bislr_payroll %>%
  filter(DPT.WRKD.LEGACY %in%
    subset(report_list, Report.ID %in% accrual_report_ids)$Cost.Center)

bislr_payroll <- bislr_payroll %>%
  mutate(
    DPT.WRKD = case_when(
      DPT.WRKD.LEGACY %in%
        subset(report_list, Report.ID %in% accrual_report_ids)$Cost.Center &
        !Department.Name.Worked.Dept %in% true_accrual_cc_desc ~
          DPT.WRKD.LEGACY,
      TRUE ~ DPT.WRKD),
    Department.Name.Worked.Dept = case_when(
      DPT.WRKD.LEGACY %in%
        subset(report_list, Report.ID %in% accrual_report_ids)$Cost.Center &
        !Department.Name.Worked.Dept %in% true_accrual_cc_desc ~
          "ACCRUAL COST CENTER",
      TRUE ~ Department.Name.Worked.Dept),
    WRKDpt_in_Dict = case_when(
      DPT.WRKD %in%
        subset(report_list, Report.ID %in% accrual_report_ids)$Cost.Center ~ 1,
      TRUE ~ WRKDpt_in_Dict),
    WRKJC_in_Dict = case_when(
      DPT.WRKD %in%
        subset(report_list, Report.ID %in%
                 accrual_report_ids)$Cost.Center ~ NA_real_,
      TRUE ~ WRKJC_in_Dict)
    )

if (nrow(bislr_payroll) != row_count) {
  showDialog(title = "Join error",
             message = paste("Row count failed at", "bislr_payroll"))
  stop(paste("Row count failed at", "bislr_payroll"))
}

## Update Universal Files --------------------------------------------------
loop <- 0
while (NA %in% unique(bislr_payroll$JC_in_UniversalFile) |
       NA %in% unique(bislr_payroll$PROVIDER)) {
  loop <- loop + 1
  new_jobcodes <- bislr_payroll %>%
    filter(is.na(JC_in_UniversalFile)) %>%
    select(Job.Code, Position.Code.Description) %>%
    unique() %>%
    mutate(JobDescCap = toupper(Position.Code.Description))

  row_count <- nrow(new_jobcodes)
  new_jobcodes <- new_jobcodes %>%
    left_join(map_uni_jobcodes %>%
      filter(PAYROLL == "MSHQ") %>%
      select(J.C.DESCRIPTION, PROVIDER, PREMIER.J.C,
             PREMIER.J.C.DESCRIPTION) %>%
      rename(JobDescCap = J.C.DESCRIPTION)) %>%
    select(-JobDescCap) %>%
    unique()
  if (nrow(new_jobcodes) != row_count) {
    showDialog(title = "Join error",
               message = paste("Row count failed at", "new_jobcodes"))
    stop(paste("Row count failed at", "new_jobcodes"))
  }

  View(new_jobcodes)


  if (nrow(new_jobcodes) > 0) {
    new_jc_path <- paste0(dir_universal, "/Mapping/BISLR payroll script")
    write.csv(new_jobcodes,
              paste0(new_jc_path,
                     "/new_jc_for_Universal_File_",
                     as.character(Sys.time(), format = "%Y-%m-%d"),
                     if (loop == 1) {
                       ""
                     }else{
                       paste0("_V", loop)
                     },
                     ".csv"))
  }

  # MM: nothing can be navigated and viewed in an R session while
  # the following window is open.  Perhaps this is an instance to
  # add an additional step to get user input in the console
  # or find another strategy
  warn_new_jc <-
    showQuestion(title = "Warning",
                 message =
                   paste0(
                     if (loop == 1) {
                       paste0("Job Code attention needed!\n",
                              "New jobcodes and/or PROVIDER value is NA.\n")
                     } else {
                       paste0("There are still new job codes and/or \n",
                              "PROVIDER values of NA.\n")
                     },
                     "Update Universal Job Code File before continuing. \n",
                     "\nIf there are new Job Codes they were written to a ",
                     "file in this directory:\n'", new_jc_path, "'\n\n",
                     "\n Has the mapping file been completely updated?",
                     "\n \n If you want to quit running the code select NO \n",
                     " then click the stop code button in the console"),
                 ok = "YES", cancel = "NO")

  if (warn_new_jc == FALSE) {
    Sys.sleep(15)
  }

  # MM: I believe Anjelica and I discussed nesting many of these statements
  # within an if() statement based on an "ok" user input from the previous
  # prompt, but there's probably a simpler solution
  map_uni_jobcodes <- read_xlsx(paste0(dir_universal,
                                       "/Mapping/MSHS_Jobcode_Mapping.xlsx"),
                                sheet = 1)
  map_uni_jobcodes <- map_uni_jobcodes %>%
    mutate(J.C = str_trim(J.C)) %>%
    mutate(JC_in_UniversalFile = 1)

  row_count <- nrow(bislr_payroll)
  bislr_payroll <- left_join(
    bislr_payroll %>%
      select(-JC_in_UniversalFile, -PROVIDER),
    map_uni_jobcodes %>%
      filter(PAYROLL == "BISLR") %>%
      select(J.C, PROVIDER, JC_in_UniversalFile) %>%
      rename(Job.Code = J.C))
  if (nrow(bislr_payroll) != row_count) {
    showDialog(title = "Join error",
               message = paste("Row count failed at",
                               "bislr_payroll new job codes"))
    stop(paste("Row count failed at", "bislr_payroll new job codes"))
  }
  Sys.sleep(2)
}

loop <- 0
while (NA %in% unique(bislr_payroll$Paycode_in_Universal)) {
  loop <- loop + 1
  new_paycodes <- bislr_payroll %>%
    filter(is.na(Paycode_in_Universal)) %>%
    select(Facility.Hospital.Id_Worked, Pay.Code) %>%
    unique()
  View(new_paycodes)

  write.csv(new_paycodes,
            paste0(new_jc_path,
                   "/new_paycodes_for_Universal_File_",
                   as.character(Sys.time(), format = "%Y-%m-%d"),
                   if (loop == 1) {
                     ""
                   } else {
                     paste0("_V", loop)
                   },
                   ".csv"))
  
  warn_new_pc <-
    showQuestion(
      title = "Warning",
      message = paste0(
        if (loop == 1) {
          "New pay codes detected! \n"

          if (max(nchar(new_paycodes$Pay.Code)) > char_len_paycode) {
            paste0("\nBe aware a paycode will need to be shortened so it's ",
                   "not longer than the ", char_len_paycode, " limit.\n")
          }

        } else {
          "There are still new pay codes. \n"

          if (max(nchar(new_paycodes$Pay.Code)) > char_len_paycode) {
            paste0("\nBe aware a paycode will need to be shortened so it's ",
                   "not longer than the ", char_len_paycode, " character ",
                   "limit.\n")
          }
        },
        "Update Universal Pay Code File before continuing. \n",
        "\n Have new pay codes been added?",
        "\n \n If you want to quit running the code select No \n",
        " then click the stop code button in the console"),
      ok = "Yes", cancel = "No")

  if (warn_new_pc == FALSE) {
    Sys.sleep(15)
  }

  map_uni_paycodes <- read_xlsx(paste0(dir_universal,
                                       "/Mapping/MSHS_Paycode_Mapping.xlsx"),
                                sheet = 1)
  map_uni_paycodes <- map_uni_paycodes %>%
    mutate(Paycode_in_Universal = 1)

  row_count <- nrow(bislr_payroll)

  bislr_payroll <- left_join(
    bislr_payroll %>%
      select(-Paycode_in_Universal),
    map_uni_paycodes %>%
      select(RAW.PAY.CODE, Paycode_in_Universal) %>%
      rename(Pay.Code = RAW.PAY.CODE))
  if (nrow(bislr_payroll) != row_count) {
    showDialog(
      title = "Join error",
      message = paste("Row count failed at", "bislr_payroll new pay codes"))
    stop(paste("Row count failed at", "bislr_payroll new pay codes"))
  }
  Sys.sleep(2)
}

## Paycycles --------------------------------------------------------------

# Paycycles to filter on

# based on the data
paycycles_data <- bislr_payroll %>%
  select(Start.Date, End.Date) %>%
  distinct() %>%
  filter(as.Date(Start.Date, "%m/%d/%Y") > dist_prev &
           as.Date(Start.Date, "%m/%d/%Y") < distribution_date)

# based on Pay_Cycle_uploaded
filter_dates <- bislr_payroll %>%
  filter(is.na(Pay_Cycle_Uploaded)) %>%
  select(Start.Date, End.Date) %>%
  unique() %>%
  arrange(Start.Date) %>%
  filter(
    End.Date > dist_prev,
    !Start.Date > distribution_date) %>%
  mutate(upload_date = 1) %>%
  arrange(Start.Date, End.Date)

# Updating pay cycles filter dates dictionary
if (nrow(filter_dates) > 0) {
  pay_cycles_uploaded <- rbind(pay_cycles_uploaded,
                               rename(filter_dates,
                                      Pay_Cycle_Uploaded = upload_date)) %>%
    select(-Pay_Cycle_Uploaded) %>%
    mutate(capture_time = as.character(Sys.time()))
  date_filtering <- filter_dates
}else{
  showDialog(
    title = "Dates in Data",
    message = paste("It appears that you're working with data",
                    "that has already been prepared for Premier upload",
                    "at some point in the past.  Be careful not to",
                    "overwrite data files by mistake."))
  date_filtering <- paycycles_data %>%
    mutate(upload_date = 1) %>%
    arrange(Start.Date, End.Date)
}

## JC ID check ----------------------------------------------------
# this section is here because if any job codes become duplicates after
# getting shortened then we need to be aware of the conflict and resolve it
# This might warrant a restructuring of the JC universal mapping file

# There's an assumption that the universal jc mapping file has been updated
# with new jobcodes by this point.

# incorporate this into the if-statement above looking at JC_in_UniversalFile?
if (exists("new_jobcodes")) {
  jc_check_new_long <- new_jobcodes %>%
    mutate(Job.Code_up = substr(Job.Code, 1, 10)) %>%
    inner_join(dict_premier_jobcode %>%
                 filter(Site %in% c("630571", "NY2162", "NY2163")) %>%
                 select(Job.Code) %>%
                 distinct(),
               by = c("Job.Code_up" = "Job.Code"))

  # This can get incorporated into Preprocessing > Update Universal Files
  # section

  if (nrow(jc_check_new_long) > 0) {
    showDialog(title = "ERROR: Job Codes",
               message = paste0("There are duplicates in ",
                                "shortened Job Codes.  ",
                                "These will require special handling.  ",
                                "Please stop the script and resolve concerns ",
                                "before restarting."))
    stop(paste0("There are duplicates in shortened Job Codes.\n\n",
                "These will require special handling.\n\n",
                "Please stop the script and resolve concerns ",
                "before restarting.\n"))
    View(jc_check_new_long)
  } else {
    cat("All shortened job codes are unique\n")
  }
}

# Formatting Outputs ---------------------------------------------------------

## Premier Payroll File ----------------------------------------------------

row_count <- nrow(bislr_payroll)
upload_payroll <- bislr_payroll %>%
  # best known alternative to a join when trying to match on multiple columns is
  # a case_when and mutate on multiple columns
  # This is kind of like creating a helper column in Excel
  # DRAFT of alternative to join:
  # mutate(upload_date = case_when(
  #   paste0(Start.Date, "-", End.Date) %in%
  #     paste0(filter_dates$Start.Date, "-", filter_dates$End.Date) ~ 1,
  #   TRUE ~ )) %>%
  # join seems simple/quick enough that we could keep it.
  left_join(date_filtering) %>%
  left_join(select(map_uni_paycodes, RAW.PAY.CODE, PAY.CODE),
            by = c("Pay.Code" = "RAW.PAY.CODE")) %>%
  mutate(Pay.Code = PAY.CODE,
         PAY.CODE = NULL,
         Start.Date = format(Start.Date, "%m/%d/%Y"),
         End.Date = format(End.Date, "%m/%d/%Y"))

if (nrow(upload_payroll) != row_count) {
  showDialog(title = "Join error",
             message = paste("Row count failed at", "upload_payroll"))
  stop(paste("Row count failed at", "upload_payroll"))
}

upload_payroll <- upload_payroll %>%
  filter(PROVIDER == 0) %>%
  filter(!is.na(upload_date)) %>%
  group_by(PartnerOR.Health.System.ID, Home.FacilityOR.Hospital.ID, DPT.HOME,
    Facility.Hospital.Id_Worked, DPT.WRKD, Start.Date, End.Date,
    Employee.ID, Employee.Name, Approved.Hours.per.Pay.Period,
    Job.Code_up, Pay.Code) %>%
  summarize(Hours = sum(Hours, na.rm = TRUE),
            Expense = sum(Expense, na.rm = TRUE)) %>%
  ungroup()

# upload_payroll will be split into BIB and SLW in the Exporting section

## Premier Reference Files -------------------------------------------------

### Dpt Dict and Map -------------------------------------------------------

if (NA %in% bislr_payroll$HomeDpt_in_Dict |
    NA %in% bislr_payroll$WRKDpt_in_Dict) {

  # update dpt dict
  upload_dict_dpt <- rbind(
    bislr_payroll %>%
      filter(is.na(HomeDpt_in_Dict)) %>%
      select(PartnerOR.Health.System.ID, Home.FacilityOR.Hospital.ID,
             DPT.HOME, Department.Name.Home.Dept) %>%
      setNames(colnames(dict_premier_dpt %>%
                          select(-Dpt_in_Dict))),
    bislr_payroll %>%
      filter(is.na(WRKDpt_in_Dict)) %>%
      select(PartnerOR.Health.System.ID, Facility.Hospital.Id_Worked,
             DPT.WRKD, Department.Name.Worked.Dept) %>%
      setNames(colnames(dict_premier_dpt %>%
                          select(-Dpt_in_Dict)))) %>%
    # check for special characters in name (e.g. ampersand &)
    # mutate(Cost.Center.Description = case_when(
    #   str_detect(Cost.Center.Description, "&") ~
    #     str_replace(Cost.Center.Description, "&", "AND"),
    #   TRUE ~ Cost.Center.Description)) %>%
    # check for cost center name length
    mutate(Cost.Center.Description =
             str_sub(Cost.Center.Description, 1, 50)) %>%
    distinct()
  # if there are no new depts, then this will be empty


  # update dpt map

  row_count <- nrow(upload_dict_dpt)
  upload_map_dpt <- upload_dict_dpt %>%
    left_join(dict_premier_dpt)
  if (nrow(upload_map_dpt) != row_count) {
    showDialog(title = "Join error",
               message = paste("Row count failed at", "upload_map_dpt"))
    stop(paste("Row count failed at", "upload_map_dpt"))
  }

  upload_map_dpt <- upload_map_dpt %>%
    filter(is.na(Dpt_in_Dict)) %>%
    mutate(Dpt_in_Dict = NULL,
           Cost.Center.Description = NULL) %>%
    mutate(effective_date = format(map_effective_date_dpt, "%m/%d/%Y"),
           prem_map = new_dpt_map) %>%
    relocate(effective_date, .before = Corporation.Code) %>%
    distinct()
}

### Dpt JC Dict and Map -----------------------------------------------------

if (NA %in% bislr_payroll$WRKJC_in_Dict |
    NA %in% bislr_payroll$HOMEJC_in_Dict) {

  # update dpt job code dict
  upload_dict_dpt_jc <- rbind(
    bislr_payroll %>%
      filter(is.na(WRKJC_in_Dict), PROVIDER == 0) %>%
      select(PartnerOR.Health.System.ID, Facility.Hospital.Id_Worked,
             DPT.WRKD, Job.Code_up, Position.Code.Description) %>%
      setNames(colnames(dict_premier_jobcode %>%
                          select(-JC_in_Dict))),
    bislr_payroll %>%
      filter(is.na(HOMEJC_in_Dict), PROVIDER == 0) %>%
      select(PartnerOR.Health.System.ID, Home.FacilityOR.Hospital.ID,
             DPT.HOME, Job.Code_up, Position.Code.Description) %>%
      setNames(colnames(dict_premier_jobcode %>%
                          select(-JC_in_Dict)))) %>%
    # for the future, we might look out for handling descriptions
    # that have special characters, such as & (ampersand)
    # mutate(Job.Code.Description = case_when(
    #   str_detect(Job.Code.Description, "&") ~
    #     str_replace(Job.Code.Description, "&", "AND"),
    #   TRUE ~ Job.Code.Description)) %>%
    mutate(Job.Code.Description =
             str_trim(str_sub(Job.Code.Description, 1, 50))) %>%
    distinct(across(-Job.Code.Description), .keep_all = TRUE)

  # update dpt job code map

  row_count <- nrow(upload_dict_dpt_jc)
  upload_map_dpt_jc <- upload_dict_dpt_jc %>%
    select(-Job.Code.Description) %>%
    # the map_premier_dpt Cost.Center column is character type
    mutate(Cost.Center = as.character(Cost.Center)) %>%
    left_join(map_premier_dpt %>%
                select(-Effective.Date)) %>%
    mutate(Cost.Center.Map = as.double(Cost.Center.Map)) %>%
    mutate(Cost.Center.Map = case_when(
      is.na(Cost.Center.Map) ~ new_dpt_map,
      TRUE ~ Cost.Center.Map)) %>%
    left_join(map_uni_jobcodes %>%
                filter(PAYROLL == "BISLR") %>%
                select(J.C, PREMIER.J.C) %>%
                mutate(J.C.prem = substr(J.C, 1, 10)) %>%
                select(-J.C),
              by = c("Job.Code" = "J.C.prem")) %>%
    mutate(effective_date = format(map_effective_date_dpt, "%m/%d/%Y"),
           alloc_pct = jc_alloc_pct) %>%
    relocate(effective_date, .before = Corporation.Code) %>%
    distinct()
  if (nrow(upload_map_dpt_jc) != row_count) {
    showDialog(
      title = "Join error",
      message = paste("Row count failed at", "upload_map_dpt_jc"))
    stop(paste("Row count failed at", "upload_map_dpt_jc"))
  }

  # FYI check:
  # This can be moved into the QC section and/or we can filter out any NA
  # PREMIER.JC rows and handle them manually
  upload_map_dpt_jc_na <- upload_map_dpt_jc %>%
    filter(is.na(PREMIER.J.C))
  View(upload_map_dpt_jc_na)
}

### Paycode Dict and Map ---------------------------------------------------


premier_missing_paycode <- anti_join(upload_payroll %>%
                                       select(Pay.Code) %>%
                                       distinct(),
                                     dict_premier_paycode %>%
                                       select(Pay.Code))

if (nrow(premier_missing_paycode) > 0) {

  # update paycode dict
  upload_dict_paycode <- premier_missing_paycode %>%
    left_join(map_uni_paycodes %>%
                select(PAY.CODE, PAY.CODE.NAME),
              by = c("Pay.Code" = "PAY.CODE")) %>%
    merge(unique(upload_payroll$Facility.Hospital.Id_Worked)) %>%
    rename(Site = y) %>%
    mutate(Corp = corp_code) %>%
    relocate(c(Corp, Site), .before = Pay.Code)

  # user should be warned if character limit is reached
  if (max(nchar(upload_dict_paycode$Pay.Code)) > char_len_paycode |
    max(nchar(upload_dict_paycode$PAY.CODE.NAME)) > char_len_paycode_name) {
    showDialog(
      title = "Paycode field error",
      message = paste0(
        "Either a paycode or paycode name has more ",
        "characters than permitted.  ",
        "Please fix them in the upload file to prevent upload errors ",
        "and in the Universal Mapping file."))
  }

  # update paycode map

  row_count <- nrow(upload_dict_paycode)
  upload_map_paycode <- upload_dict_paycode %>%
    select(-PAY.CODE.NAME) %>%
    left_join(map_uni_paycodes %>%
                select(PAY.CODE, PAY.CODE.CATEGORY, INCLUDE.HOURS,
                       INCLUDE.EXPENSES),
              by = c("Pay.Code" = "PAY.CODE")) %>%
    mutate(eff_date = as.character(dist_prev - lubridate::days(6), "%m/%d/%Y"),
           alloc_pct = 100) %>%
    relocate(eff_date, .before = Corp)

  if (nrow(upload_map_paycode) != row_count) {
    showDialog(title = "Join error",
               message = paste("Row count failed at", "upload_map_paycode"))
    stop(paste("Row count failed at", "upload_map_paycode"))
  }
}


### Dummy Report Dict ------------------------------------------------------

# These reports are those that have the Cost Center mapped when the
# Home Cost Center is not in an existing report.  If the home cost center
# is not in an existing report, the Premier Employee Level Detail report
# will not display the employee's hour detail.


upload_report_dict <- bislr_payroll %>%
  # Home Facility is used because we're mainly interested in the
  # Home Department for the dummy reports that are being updated
  select(Home.FacilityOR.Hospital.ID, DPT.HOME, DPT.WRKD) %>%
  left_join(report_list %>%
              filter(Report.ID %in% dummy_report_ids) %>%
              select(Site, Cost.Center) %>%
              rename(Facility.Hospital.Id_Worked = Site,
                     DPT.WRKD = Cost.Center) %>%
              mutate(WRKD.DPT.in.Report = 1)) %>%
  left_join(report_list %>%
              filter(Report.ID %in% dummy_report_ids) %>%
              select(Site, Cost.Center) %>%
              rename(Home.FacilityOR.Hospital.ID = Site,
                     DPT.HOME = Cost.Center) %>%
              mutate(HOME.DPT.in.Report = 1)) %>%
  filter(WRKD.DPT.in.Report == 1, is.na(HOME.DPT.in.Report)) %>%
  select(-DPT.WRKD, -Facility.Hospital.Id_Worked, -WRKD.DPT.in.Report,
         -HOME.DPT.in.Report) %>%
  rename(Site = Home.FacilityOR.Hospital.ID, Cost.Center = DPT.HOME) %>%
  rbind(report_list %>%
          filter(Report.ID %in% dummy_report_ids) %>%
          select(Site, Cost.Center)) %>%
  unique()

upload_report_dict_qc <- upload_report_dict %>%
  group_by(Site) %>%
  summarize(Cost.Center = paste(Cost.Center, collapse = "; ")) %>%
  mutate(num_char = nchar(Cost.Center))

if (max(upload_report_dict_qc$num_char >= 3000)) {
  showDialog(
    title = "Dummy Report Cost Centers Overflow",
    message = paste0("There are too many characters in the Dummy Report ",
                     "Cost Center list.  If you upload this file and try ",
                     "to publish, the publishing job will stall and break ",
                     "Premier.  PLEASE PLEASE PLEASE reduce the number of ",
                     "Cost Centers included in the report before Publishing ",
                     "Premier data.")
    )
}

upload_report_dict <- upload_report_dict %>%
  group_by(Site) %>%
  summarize(Cost.Center = paste(Cost.Center, collapse = ":")) %>%
  left_join(dict_premier_report %>%
    filter(Report.ID %in% dummy_report_ids) %>%
    select(-contains("blank"), -Cost.Center) %>%
    distinct_at(vars(Site), .keep_all = T)) %>%
  relocate(Site, .after = Corporation.Code) %>%
  relocate(Cost.Center, .after = Report.ID)

# Quality Checks -------------------------------------------------------


## JC with multiple Description -------------------------------------------

jc_desc_check <- bislr_payroll %>%
  select(Job.Code, Position.Code.Description) %>%
  distinct() %>%
  group_by(Job.Code) %>%
  summarize(freq = n()) %>%
  arrange(-freq, Job.Code) %>%
  filter(freq > 1) %>%
  inner_join(bislr_payroll %>%
    select(Job.Code, Position.Code.Description, PROVIDER) %>%
    distinct())

# 5 is selected because of the DUS_RMV jobcode
if (max(jc_desc_check$freq) > jc_desc_threshold) {
  showDialog(
    title = "Jobcode Description Check",
    message = paste(
      "There are a large number of descriptions",
      "mapped to the same jobcode. ",
      "Review the jobcodes with multiple",
      "descriptions before proceeding."
    ))
  View(jc_desc_check)
}
# pull in Premier jobcode category or other info to help?
# seems like it will be infrequent, so it's not be worth coding for it at
# this point in time


## cost center and upload FTE count -------------------------------------

# average FTEs since previous distribution

# need Pay Code info to get appropriate hours
row_count <- nrow(upload_payroll)
fte_summary <- upload_payroll %>%
  left_join(map_uni_paycodes %>%
    select(PAY.CODE, INCLUDE.HOURS, WORKED.PAY.CODE) %>%
    distinct(),
  by = c("Pay.Code" = "PAY.CODE"))

if (nrow(fte_summary) != row_count) {
  showDialog(title = "Join error",
             message = paste("Row count failed at", "fte_summary"))
  stop(paste("Row count failed at", "fte_summary"))
}

fte_summary <- fte_summary %>%
  filter(INCLUDE.HOURS == 1) %>%
  mutate(Hours_Worked = WORKED.PAY.CODE * Hours) %>%
  group_by(Facility.Hospital.Id_Worked, DPT.WRKD) %>%
  summarize(
    Avg_FTEs_worked =
      round(sum(Hours_Worked, na.rm = TRUE) /
              (37.5 * (as.numeric(distribution_date - dist_prev) / 7)), 1),
    Avg_FTEs_paid =
      round(sum(Hours, na.rm = TRUE) /
              (37.5 * (as.numeric(distribution_date - dist_prev) / 7)), 1)) %>%
  ungroup() %>%
  mutate(dist_date = format(distribution_date, "%m/%d/%Y")) %>%
  relocate(dist_date, .before = Avg_FTEs_worked) %>%
  mutate(capture_time = as.character(Sys.time())) %>%
  rename(Site = Facility.Hospital.Id_Worked, Department = DPT.WRKD) %>%
  mutate(Site = case_when(
    Site == "630571" ~ "MSBIB",
    Site == "NY2162" ~ "MSW",
    Site == "NY2163" ~ "MSM",
    TRUE ~ "Other"))

fte_summary_path <- paste0("//researchsan02b/shr2/deans/Presidents/",
                           "SixSigma/MSHS Productivity/Productivity/",
                           "Labor - Data/Multi-site/BISLR/Quality Checks/",
                           "Source Data/")

fte_summary <- rbind(fte_summary,
                     read.xlsx2(file = paste0(fte_summary_path,
                                              "fte_summary.xlsx"),
                                colClasses = c(rep("character", 9),
                                               rep("numeric", 2),
                                               "character"),
                                sheetName = "fte_summary") %>%
                       select(Site, Department, dist_date,
                              Avg_FTEs_worked, Avg_FTEs_paid,
                              capture_time))

if (fte_summary %>% select(dist_date) %>% distinct() %>% nrow() !=
    fte_summary %>% select(dist_date, capture_time) %>% distinct() %>% nrow()) {
  showDialog(title = "Warning",
             message = paste(
               "You will likely be appending rows to",
               "the fte_summary file for a distribution",
               "period that already exists in the file."))
  stop(paste("You will likely be appending rows to",
             "the fte_summary file for a distribution",
             "period that already exists in the file."))
}

row_count <- nrow(fte_summary)
fte_summary <- fte_summary %>%
  left_join(map_uni_reports %>%
              filter(is.na(CLOSED) & DEPARTMENT.BREAKDOWN == 1) %>%
              select(DEFINITION.CODE, DEFINITION.NAME, ORACLE.COST.CENTER,
                     SERVICE.LINE, CORPORATE.SERVICE.LINE, VP) %>%
              distinct(),
            by = c("Department" = "ORACLE.COST.CENTER")) %>%
  left_join(rbind(
    dict_premier_dpt %>%
      select(-Dpt_in_Dict) %>%
      mutate(Site = case_when(
        Site == "630571" ~ "MSBIB",
        Site == "NY2162" ~ "MSW",
        Site == "NY2163" ~ "MSM",
        TRUE ~ "Other")),
    upload_dict_dpt) %>%
      select(-Corporation.Code),
    by = c("Site" = "Site", "Department" = "Cost.Center")) %>%
  select(Site, Department, Cost.Center.Description,
         DEFINITION.CODE, DEFINITION.NAME, SERVICE.LINE,
         CORPORATE.SERVICE.LINE, VP, dist_date,
         Avg_FTEs_worked, Avg_FTEs_paid, capture_time)

if (nrow(fte_summary) != row_count) {
  showDialog(title = "Join error",
             message = paste("Row count failed at", "fte_summary"))
  stop(paste("Row count failed at", "fte_summary"))
}

## 8600 Accrual Site Summary --------------------------------------------

accrual_summary <- bislr_payroll %>%
  left_join(date_filtering) %>%
  filter(PROVIDER == 0) %>%
  filter(DPT.WRKD %in%
           subset(report_list,
                  Report.ID %in% accrual_report_ids)$Cost.Center) %>%
  group_by(Facility.Hospital.Id_Worked, DPT.WRKD, Start.Date, End.Date) %>%
  summarize(Hours = sum(Hours, na.rm = TRUE),
            Expense = sum(Expense, na.rm = TRUE)) %>%
  ungroup()

# row count check not required for joining date_filtering
accrual_raw_detail <- accrual_raw_detail %>%
  left_join(date_filtering)

row_count <- nrow(accrual_raw_detail)
accrual_raw_detail <- accrual_raw_detail %>%
  left_join(map_uni_paycodes %>%
              select(RAW.PAY.CODE, INCLUDE.HOURS, INCLUDE.EXPENSES,
                     WORKED.PAY.CODE),
            by = c("Pay.Code" = "RAW.PAY.CODE")) %>%
  mutate(INCLUDE.HOURS = as.integer(INCLUDE.HOURS),
         INCLUDE.EXPENSES = as.integer(INCLUDE.EXPENSES),
         WORKED.PAY.CODE = as.integer(WORKED.PAY.CODE))
if (nrow(accrual_raw_detail) != row_count) {
  showDialog(title = "Join error",
             message = paste("Row count failed at", "accrual_raw_detail"))
  stop(paste("Row count failed at", "accrual_raw_detail"))
}

row_count <- nrow(accrual_raw_detail)
accrual_raw_summary <- accrual_raw_detail %>%
  mutate(Hours_Worked = Hours * INCLUDE.HOURS * WORKED.PAY.CODE,
         Expense_Worked = Expense * INCLUDE.EXPENSES * WORKED.PAY.CODE,
         Hours_Paid = Hours * INCLUDE.HOURS,
         Expense_Paid = Expense * INCLUDE.EXPENSES) %>%
  group_by(Facility.Hospital.Id_Worked, DPT.WRKD, Department.Name.Worked.Dept,
           DPT.WRKD.LEGACY, Start.Date, End.Date) %>%
  summarize(Hours_Worked = sum(Hours_Worked, na.rm = TRUE),
            Expense_Worked = sum(Expense_Worked, na.rm = TRUE),
            Hours_Paid = sum(Hours_Paid, na.rm = TRUE),
            Expense_Paid = sum(Expense_Paid, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(map_uni_reports %>%
              select(ORACLE.COST.CENTER, DEFINITION.CODE, DEFINITION.NAME),
            by = c("DPT.WRKD" = "ORACLE.COST.CENTER"))
if (nrow(accrual_raw_detail) != row_count) {
  showDialog(title = "Join error",
             message = paste("Row count failed at", "accrual_raw_summary"))
  stop(paste("Row count failed at", "accrual_raw_summary"))
}

View(accrual_raw_summary)


# Exporting Data ----------------------------------------------------------

date_range <- paste0(
  format(dist_prev + lubridate::days(1), "%Y-%m-%d"),
  "_to_",
  format(distribution_date, "%Y-%m-%d")
)

## Reference Files --------------------------------------------------------

write.table(upload_dict_dpt,
            file = paste0(dir_BISLR, "/BISLR_Department Dictionary_",
                          date_range, ".csv"),
            row.names = F, col.names = F, sep = ",")


write.table(upload_map_dpt,
            file = paste0(dir_BISLR, "/BISLR_Department Map_",
                          date_range, ".csv"),
            row.names = F, col.names = F, sep = ",")


write.table(upload_dict_dpt_jc,
            file = paste0(dir_BISLR, "/BISLR_Department Job Code Dictionary_",
                          date_range, ".csv"),
            row.names = F, col.names = F, sep = ",")


write.table(upload_map_dpt_jc,
            file = paste0(dir_BISLR, "/BISLR_Department Job Code Map_",
                          date_range, ".csv"),
            row.names = F, col.names = F, sep = ",")


if (exists("new_paycodes")) {
  write.table(upload_dict_paycode,
              file = paste0(dir_BISLR, "/BISLR_Pay Code Dictionary_",
                            date_range, ".csv"),
              row.names = F, col.names = F, sep = ",")

  write.table(upload_map_paycode,
              file = paste0(dir_BISLR, "/BISLR_Pay Code Map_",
                            date_range, ".csv"),
              row.names = F, col.names = F, sep = ",")

}

write.table(upload_report_dict,
            file = paste0(dir_BISLR, "/BISLR_Dummy Report Dictionary_",
                          date_range, ".csv"),
            row.names = F, col.names = F, sep = ",")

if (nrow(filter_dates) > 0) {
  write.xlsx(pay_cycles_uploaded,
             file = paste0(dir_BISLR, "/Reference",
                           "/Pay cycles uploaded_Tracker", ".xlsx"),
             row.names = F)
}

## Payroll Files --------------------------------------------------------------

# MM: is there a better way to cycle than to use sapply?
# or should this simply be muted so the NULL return is not printed in
# the console?
sapply(
  seq_len(length(unique(upload_payroll$Facility.Hospital.Id_Worked))),
  function(x) {
    write.table(
      filter(upload_payroll,
             Facility.Hospital.Id_Worked ==
               unique(upload_payroll$Facility.Hospital.Id_Worked)[x]),
      file = paste0(dir_BISLR, "/",
                    unique(upload_payroll$Facility.Hospital.Id_Worked)[x],
                    "_Payroll_", date_range, ".csv"),
      row.names = F, col.names = F, sep = ",")
  }
)

## Quality Files --------------------------------------------------------------

# accrual detail
write_rds(accrual_raw_detail,
          file = paste0(dir_BISLR,
                        "/Quality Checks/Accrual Issue Raw Payroll",
                        "/Raw Accrual Original Payroll",
                        date_range, ".rds"))

# backup the previous fte_summary
file.rename(from = paste0(fte_summary_path, "fte_summary.xlsx"),
            to = paste0(fte_summary_path,
                        "fte_summary_BU_",
                        as.character(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"),
                        ".xlsx"))
# fte_summary
write.xlsx2(as.data.frame(fte_summary),
            file = paste0(fte_summary_path, "fte_summary.xlsx"),
            row.names = F,
            sheetName = "fte_summary",
            append = FALSE)

# rename previous piv_wide_check
file.rename(from = paste0(dir_BISLR, "/Quality Checks",
                          "/piv_wide_check", ".csv"),
            to = paste0(dir_BISLR, "/Quality Checks",
                        "/piv_wide_check_BU_",
                        as.character(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"),
                        ".csv"))
# write piv_wide_check
write.csv(piv_wide_check,
          file = paste0(dir_BISLR, "/Quality Checks",
                        "/piv_wide_check", ".csv"),
          row.names = FALSE)

# End of Script -----------------------------------------------------------
