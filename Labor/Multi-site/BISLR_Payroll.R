
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(xlsx)
library(rstudioapi)
library(stringr)
library(stringi)

# Directories -------------------------------------------------------------
dir <- 'J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity'
dir_BISLR <- paste0(dir, '/Labor - Data/Multi-site/BISLR')
dir_universal <- paste0(dir, '/Universal Data')
  
# Constants ---------------------------------------------------------------
new_dpt_map <- 10095
map_effective_date <- as.Date('2022-01-01') #is this date ok?
# MM: I typically use an older date like 1/1/2010
# this way any remapping we perform will ensure it's incorporated
# into data
accural_legacy_cc <- c(1109008600, 1109028600, 4409008600, 6409008600) #add other 8600, make quality check for new 8600, id errors non accural oracle but backmapped accural
productive_paycodes <- c('REGULAR', 'OVERTIME', 'EDUCATION', 'ORIENTATION',
                        'OTHER_WORKED', 'AGENCY')

# general improvement opportunity:
# can we update the paycode mapping file to indicate productive vs. non-prod?

dummy_report_ids <- c('DNU_000', 'DNU_MSM000', 'DNU_MSW000')


  ## Premier Formatting ------------------------------------------------------
  char_len_dpt <- 15
  char_len_dpt_name <- 50
  char_len_employ <- 30
  char_len_jobcode <- 10
  char_len_paycode <- 15
  char_len_paycode_name <- 50

# Functions --------------------------------------------------------------
  import_recent_file <- function(folder.path, place) {
  #Importing File information from Folder
  File.Name <- list.files(path = folder.path, full.names = F)
  File.Path <- list.files(path = folder.path, full.names = T)
  File.Date <- as.Date(sapply(File.Name,
                              function(x)
                                if(substr(x, nchar(x)-3, nchar(x)) == '.txt'){
                                  substr(x,nchar(x)-18, nchar(x)-9)
                                }else(substr(x,nchar(x)-14, nchar(x)-5))),
                       format = '%m_%d_%Y')
  File.Table <<- data.table::data.table(File.Name, File.Date, File.Path) %>%
    arrange(desc(File.Date))
  #Quality Check - Confirming Upload File
  answer <- showQuestion(title = 'Question',
                         message = paste0('File selected is: \n',
                                          File.Table$File.Name[place],
                                          '. \nIs this the correct file?'),
                         ok = 'Yes',
                         cancel = 'No')
  if (answer == F) {
    user_selected_file <- select.list(choices = File.Table$File.Name,
                                multiple = F,
                                title = 'Select the correct file',
                                graphics = T)
    place <- grep(user_selected_file, File.Table$File.Name)
    cat('File selected is ', File.Table$File.Name[place])
  }
  #Importing Data 
  data_recent <- read.csv(File.Table$File.Path[place],
                            header = T,
                            sep = '~',
                            fill = T)
                            # colClasses = "character")
                            # Home dept came in as numeric and was displaying
                            # as scientific, so tried bringing in as text
                            # since all other columns were text.
                            # Also found that some values are not including
                            # location and dept code

  # Also seeing:  
  # Warning message:
  #   In scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :
  #             EOF within quoted string

  return(data_recent)
  }

# Import Data -------------------------------------------------------------
raw_payroll <- import_recent_file(paste0(dir_BISLR, '/Source Data'), 1)

# Import References -------------------------------------------------------
pay_cycles_uploaded <- read.xlsx(paste0(dir_BISLR,
                                        '/Reference',
                                        '/Pay cycles uploaded_Tracker.xlsx'),
                                 sheetIndex = 1)
msus_removal_list <- read_xlsx(paste0(dir_BISLR,
                                      '/Reference/MSUS_removal_list.xlsx'),
                               sheet = 1)
  ## Universal Reference Files -----------------------------------------------
  map_uni_paycodes <- read_xlsx(paste0(dir_universal,
                                       '/Mapping/MSHS_Paycode_Mapping.xlsx'),
                                sheet = 1)
  map_uni_jobcodes <- read_xlsx(paste0(dir_universal,
                                       '/Mapping/MSHS_Jobcode_Mapping.xlsx'),
                                sheet = 1)
  map_uni_reports <- read_xlsx(paste0(dir_universal,
                                      '/Mapping/MSHS_Reporting_Definition_Mapping.xlsx'),
                               sheet = 1)
  map_uni_paycycles <- read_xlsx(paste0(dir_universal,
                                        '/Mapping/MSHS_Pay_Cycle.xlsx'),
                                 sheet = 1)

  ## Premier Reference Files -------------------------------------------------
  dict_premier_dpt <- read.csv(paste0(dir_universal,
                                      '/Premier/Dictionary Exports',
                                      '/DepartmentDictionary.csv'),
                               col.names = c('Corporation.Code',
                                             'Site',
                                             'Cost.Center',
                                             'Cost.Center.Description'),
                               sep = ',')
  map_premier_dpt <- read.csv(paste0(dir_universal,
                                     '/Premier/Mapping Exports',
                                     '/DepartmentMapping.csv'),
                              col.names = c('Effective.Date',
                                            'Corporation.Code',
                                            'Site',
                                            'Cost.Center',
                                            'Cost.Center.Map'),
                              sep = ',')
  dict_premier_jobcode <- read.csv(paste0(dir_universal,
                                          '/Premier/Dictionary Exports',
                                          '/DeptJobCodeDictionary.csv'),
                                   col.names = c('Corporation.Code',
                                                 'Site',
                                                 'Cost.Center',
                                                 'Job.Code',
                                                 'Job.Code.Description'),
                                   sep = ',')
  dict_premier_report <- read.csv(paste0(dir_universal,
                                         '/Premier/Dictionary Exports',
                                         '/DepartmentDef.csv'),
                                  col.names = c('Corporation.Code',
                                                'Site',
                                                'Report.Name',
                                                'Report.ID',
                                                'Cost.Center',
                                                'Report.Type',
                                                'Threshold.Type',
                                                'Target.Type',
                                                'Exclude.Report.Rollup',
                                                'Effective.Date',
                                                'Paycycle.Type',
                                                'Exclude.Admin.Rollup',
                                                'Exclude.Action.Plan',
                                                'blank14',
                                                'blank15',
                                                'blank16',
                                                'blank17'),
                                  sep = ',',
                                  fill = T)


# Preprocessing --------------------------------------------------------------


  ## References --------------------------------------------------------------
  map_uni_jobcodes <- map_uni_jobcodes %>%
    mutate(JC_in_UnivseralFile = 1)
  pay_cycles_uploaded <- pay_cycles_uploaded %>%
    mutate(Pay_Cycle_Uploaded = 1)
  dict_premier_dpt <- dict_premier_dpt %>%
    mutate(Cost.Center = as.character(Cost.Center),
           Dpt_in_Dict = 1)
  dict_premier_jobcode <- dict_premier_jobcode %>%
    mutate(JC_in_Dict = 1)
  
  dummy_reports <- dict_premier_report %>%
    filter(Report.ID %in% dummy_report_ids) 
  dummy_reports_dept <- str_split(dummy_reports$Cost.Center,
                                  pattern = ':',
                                  simplify = T) %>%
    as.data.frame()
  dummy_report_list <- lapply(1:nrow(dummy_reports_dept),
                              function(x) pivot_longer(dummy_reports_dept[x,],
                                                       cols = everything()))
  dummy_report_list  <- lapply(1:length(dummy_report_list),
                               function(x) mutate(dummy_report_list[[x]],
                                                  Site = dummy_reports$Site[x]))
  dummy_report_list  <- do.call(rbind, dummy_report_list)
  dummy_reports <- left_join(dummy_reports,
                             dummy_report_list %>% select(Site, value)) %>%
    select(-contains('blank'), - Cost.Center) %>%
    relocate(value, .after = Report.ID) %>%
    rename(Cost.Center = value) %>%
    unique()
  rm(dummy_reports_dept, dummy_report_list)


  dist_dates <- map_uni_paycycles %>%
    select(END.DATE, PREMIER.DISTRIBUTION) %>%
    distinct() %>%
    drop_na() %>%
    arrange(END.DATE) %>%
    filter(PREMIER.DISTRIBUTION %in% c(TRUE, 1),
           END.DATE < max(
             as.POSIXct(raw_payroll$End.Date, format = "%m/%d/%Y")))
  
  #Selecting the most recent distribution date
  distribution_date <- max(as.POSIXct(dist_dates$END.DATE))
  
  dist_prev <- dist_dates$END.DATE[
    which(dist_dates$END.DATE == distribution_date) - 1]

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
    pivot_wider(names_from = End.Date,
                values_from = Hours)
  
  View(piv_wide_check)
  

  ## Data  --------------------------------------------------------------------

  bislr_payroll <- raw_payroll
  
  bislr_payroll <- bislr_payroll %>%
    mutate(DPT.WRKD = paste0(substr(Full.COA.for.Worked,1,3),
                             substr(Full.COA.for.Worked,41,44),
                             substr(Full.COA.for.Worked,5,7),
                             substr(Full.COA.for.Worked,12,16)),
           DPT.HOME = paste0(substr(Full.COA.for.Home,1,3),
                             substr(Full.COA.for.Home,41,44),
                             substr(Full.COA.for.Home,5,7),
                             substr(Full.COA.for.Home,12,16)),
           DPT.WRKD.LEGACY = paste0(substr(Reverse.Map.for.Worked, 1, 4),
                                    substr(Reverse.Map.for.Worked, 13, 14),
                                    substr(Reverse.Map.for.Worked, 16, 19)),
           DPT.HOME.LEGACY = paste0(substr(Reverse.Map.for.Home, 1, 4),
                                    substr(Reverse.Map.for.Home, 13, 14),
                                    substr(Reverse.Map.for.Home, 16, 19)),
           Start.Date = as.Date(Start.Date, format = '%m/%d/%Y'),
           End.Date = as.Date(End.Date, format = '%m/%d/%Y'),
           Employee.Name = substr(Employee.Name, 1, 30),
           Approved.Hours.per.Pay.Period = round(Approved.Hours.per.Pay.Period,
                                                 digits = 0)) %>%
    mutate(DPT.WRKD = case_when(
      DPT.WRKD.LEGACY %in% accural_legacy_cc ~ DPT.WRKD.LEGACY,
      TRUE ~ DPT.WRKD),
      Job.Code = case_when(
        paste0(DPT.WRKD, '-', Employee.Name) %in%
          paste0(msus_removal_list$`Department IdWHERE Worked`,
                 '-', msus_removal_list$`Employee Name`)
        ~ unique(msus_removal_list$`New Job Code`),
        TRUE ~ Job.Code)) %>%
    # MM: may want to change the Position.Code.Description for DUS_RMV
    # to DUS_REMOVE to prevent issues that might arise
    left_join(pay_cycles_uploaded) %>%
    left_join(map_uni_jobcodes %>% 
                filter(PAYROLL == 'BISLR') %>%
                select(J.C, PROVIDER, JC_in_UnivseralFile) %>%
                rename(Job.Code = J.C)) %>%
    left_join(map_uni_paycodes %>% 
                select(RAW.PAY.CODE) %>%
                mutate(Paycode_in_Universal = 1) %>%
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
    mutate(Job.Code_up = substr(Job.Code, 1, 10)) %>%
    mutate(Position.Code.Description = str_trim(Position.Code.Description))
  
    ## Update Universal Files --------------------------------------------------
    if (NA %in% unique(bislr_payroll$JC_in_UnivseralFile)) {
      new_jobcodes <- bislr_payroll %>%
        filter(is.na(JC_in_UnivseralFile)) %>%
        # does Job.Code_up need to be included in this select()?
        select(Job.Code, Position.Code.Description) %>%
        unique() %>%
        mutate(JobDescCap = toupper(Position.Code.Description)) %>%
        left_join(map_uni_jobcodes %>% #update so ignors case of string
                    filter(PAYROLL == 'MSHQ') %>%
                    select(J.C.DESCRIPTION, PROVIDER, PREMIER.J.C,
                           PREMIER.J.C.DESCRIPTION) %>%
                    rename(JobDescCap = J.C.DESCRIPTION)) %>%
        select(-JobDescCap) %>%
        unique()
      View(new_jobcodes)
      write.csv(new_jobcodes, 'New Job Codes for Universal File.csv')
      
      # MM: how are there new job codes when running this on July data?
      # the job codes appear in July pay cycles
      # MM: expect that the new Premier dict had not been downloaded yet
      
      # FYI CHECK:
      new_jc_check <- bislr_payroll %>%
        filter(Job.Code %in% new_jobcodes$Job.Code)
      View(new_jc_check)
      ###
      
      stop('New job codes detected, update universal job code dictionary before continuing to run code')
      # the stop didn't stop code when highlighting a large chunk of code to run
      # when new job codes existed
    }

    if (NA %in% unique(bislr_payroll$Paycode_in_Universal)) {
      new_paycodes <- bislr_payroll %>%
        filter(is.na(Paycode_in_Universal)) %>%
        select(Facility.Hospital.Id_Worked, Pay.Code) %>%
        unique() %>%
      View(new_paycodes)
      write.csv(new_paycodes, 'New Pay Codes for Universal File.csv')
      stop(paste0('New pay codes detected, update universal job code dictionary before continuing',
                  'Continue running code from line TBD.'))
    }
  
  #Paycycles to filter on - remember to update the reference file with these dates
  filter_dates <- bislr_payroll %>%
    filter(is.na(Pay_Cycle_Uploaded)) %>%
    select(Start.Date, End.Date) %>%
    unique() %>%
    arrange(Start.Date) %>%
    filter(End.Date > dist_prev,
           !Start.Date > distribution_date) %>%
    mutate(upload_date = 1)

# Formatting Outputs ---------------------------------------------------------
  
  ## JC ID check ----------------------------------------------------
  # this section is here because if any job codes become duplicates after
  # getting shortened then we need to be aware of the conflict and resolve it
  # This might result in a restructuring of the JC universal mapping file
  
  # There's an assumption that the universal jc mapping file has been updated
  # with new jobcodes by this point.

  map_uni_jobcodes_bislr <- map_uni_jobcodes %>%
    filter(PAYROLL == "BISLR") %>%
    mutate(J.C.DESCRIPTION = str_trim(J.C.DESCRIPTION)) %>%
    mutate(J.C.length = nchar(J.C),
           J.C.prem = substr(J.C, 1, 10)) %>%
    distinct()
  
  long_jc <- map_uni_jobcodes_bislr %>%
    filter(J.C.length > 10) %>%
    group_by(J.C.prem) %>%
    summarize(freq = n())
  
  if (sum(long_jc$freq) > length(long_jc$freq)) {
    showDialog(title = "ERROR: Job Codes",
               message = paste0("There are duplicates in ",
                                "shortened Job Codes.  ",
                                "These will require special handling.  ",
                                "Please stop the script and resolve concerns ",
                                "before restarting.")
    )
    stop(paste0("There are duplicates in shortened Job Codes.\n\n",
                "These will require special handling.\n\n",
                "Please stop the script and resolve concerns ",
                "before restarting.\n"))
  } else {
    cat("All shortened job codes are unique\n")
  }
  
  
  # We can identify new jobcodes by comparing with the dict_premier_jobcode
  # data.frame.
  # But it seems like maybe the Premier Dictionary is not completely up to date
  # We use the dict_premier_jobcode file to identify deptartment-jobcode
  # combinations that don't exist.
  
  # dict_premier_jobcode_bislr <- dict_premier_jobcode %>%
  # filter(Site %in% c("630571", "NY2162", "NY2163"))
  # 
  # new_jobcodes2 <- bislr_payroll %>%
  #   filter(!Job.Code %in% dict_premier_jobcode_bislr$Job.Code) %>%
  #   select(Job.Code, Position.Code.Description) %>%
  #   unique()
  # View(new_jobcodes2)

  ## Premier Payroll File ----------------------------------------------------
  
  upload_payroll <- bislr_payroll %>%
    # is there a method to filter on multiple columns instead of join?
    left_join(filter_dates) %>%
    filter(!is.na(upload_date)) %>%
    # need to consider mapping of Providers
    # could set these as separate files to manually manipulate
    # check to see how MSHQ handles this
    # IT IS CRITICAL THAT WE KNOW IF NEW JOBCODES ARE PROVIDERS
    filter(Job.Code_up != "DUS_RMV" & PROVIDER %in% c(NA, 0)) %>%
    # filter(Job.Code_up != "DUS_RMV" & PROVIDER == 0) %>%
    group_by(
      PartnerOR.Health.System.ID,
      Home.FacilityOR.Hospital.ID, DPT.HOME,
      Facility.Hospital.Id_Worked, DPT.WRKD,
      Start.Date, End.Date,
      Employee.ID, Employee.Name,
      Approved.Hours.per.Pay.Period,
      Job.Code_up,
      # PROVIDER, # uncomment this line to confirm the Providers are filtered out
      Pay.Code) %>%
    summarize(Hours = sum(Hours, na.rm = TRUE),
              Expense = sum(Expense, na.rm = TRUE)) %>%
    ungroup() %>%
    # the paycode that is used in Premier needs to be used instead of the
    # raw paycode, so it needs to be joined in.
    left_join(
      select(map_uni_paycodes, RAW.PAY.CODE, PAY.CODE),
      by = c("Pay.Code" = "RAW.PAY.CODE")) %>%
    mutate(Pay.Code = PAY.CODE,
           PAY.CODE = NULL,
           Start.Date = format(Start.Date, "%m/%d/%Y"),
           End.Date = format(End.Date, "%m/%d/%Y"))
  
  # check for confirming correct payperiods are included
  # check <- upload_payroll %>%
  #   select(Start.Date, End.Date) %>%
  #   distinct() %>%
  #   arrange(Start.Date, End.Date)

  
  ## Premier Reference Files -------------------------------------------------
  
  # update dpt dict
  
  # we should ensure that the dict_premier_jobcode file is actually up to date.
  # it won't be awful if it's not, we'll just upload more than we need to.
  # MSHQ and Rightsourcing upload everything without comparing to historical
  
  # is there a more efficient way to do this 2x?
  
  payroll_home_dpt <- bislr_payroll %>%
    filter(is.na(HomeDpt_in_Dict)) %>%
    select(PartnerOR.Health.System.ID, Home.FacilityOR.Hospital.ID,
           DPT.HOME, Department.Name.Home.Dept) %>%
    distinct()
  
  payroll_wrk_dpt <- bislr_payroll %>%
    filter(is.na(WRKDpt_in_Dict)) %>%
    select(PartnerOR.Health.System.ID, Facility.Hospital.Id_Worked,
           DPT.WRKD, Department.Name.Worked.Dept) %>%
    distinct()
  
  dpt_dict_names <- c("Corporation.Code", "Site",
                      "Cost.Center", "Cost.Center.Description")
  colnames(payroll_home_dpt) <- dpt_dict_names
  colnames(payroll_wrk_dpt) <- dpt_dict_names
  
  
  # need to consider if there are no new departments?  
  upload_dict_dpt <- rbind(payroll_home_dpt, payroll_wrk_dpt) %>%
    distinct() %>%
    # check for special characters in name (e.g. ampersand &)
    mutate(Cost.Center.Description = case_when(
      str_detect(Cost.Center.Description, "&") ~ 
        str_replace(Cost.Center.Description, "&", "AND"),
      TRUE ~ Cost.Center.Description)) %>%
    # check for cost center name length
    mutate(Cost.Center.Description =
             str_sub(Cost.Center.Description, 1, 50)) %>%
    # must remove accrual cost center IDs since they'll already be in Premier
    filter(!Cost.Center %in% accural_legacy_cc) %>%
    # or need to change the description of the cost center before uploading
    # mutate(Cost.Center.Description = case_when(
    #   Cost.Center %in% accural_legacy_cc ~ "ACCRUAL COST CENTER",
    #   TRUE ~ Cost.Center.Description)) %>%
    distinct()

  # there's some sort of error in the Cost.Center id column
  # these are values: --1--83-000 & --1--85-000
  # when running the July data
  
  # rm(payroll_home_dpt, payroll_wrk_dpt, dpt_dict_names)

  # update dpt map
  upload_map_dpt <- upload_dict_dpt %>%
    left_join(dict_premier_dpt) %>%
    filter(is.na(Dpt_in_Dict)) %>%
    mutate(Dpt_in_Dict = NULL,
           Cost.Center.Description = NULL) %>%
    mutate(effective_date = format(map_effective_date, "%m/%d/%Y"),
           prem_map = new_dpt_map) %>%
    relocate(effective_date, .before = Corporation.Code) %>%
    distinct()

  # update dpt job code dict
  
  # need to consider if there are no new departments? 

  # is there a more efficient way to do this 2x?
  upload_dict_dpt_jc_wrk <- bislr_payroll %>%
    mutate(Job.Code = substr(Job.Code, 1, 10)) %>%
    filter(Job.Code_up != "DUS_RMV" & PROVIDER %in% c(NA, 0)) %>%
    filter(is.na(WRKJC_in_Dict)) %>%
    select(PartnerOR.Health.System.ID,
           Facility.Hospital.Id_Worked, DPT.WRKD,
           Job.Code_up, Position.Code.Description) %>%
    distinct()
  
  upload_dict_dpt_jc_home <- bislr_payroll %>%
    mutate(Job.Code = substr(Job.Code, 1, 10)) %>%
    filter(Job.Code_up != "DUS_RMV" & PROVIDER %in% c(NA, 0)) %>%
    filter(is.na(HOMEJC_in_Dict)) %>%
    select(PartnerOR.Health.System.ID,
           Home.FacilityOR.Hospital.ID, DPT.HOME,
           Job.Code_up, Position.Code.Description) %>%
    distinct()
  
  jc_dict_names <- c("Corporation.Code", "Site",
                     "Cost.Center", "Job.Code", "Job.Code.Desc")
  colnames(upload_dict_dpt_jc_wrk) <- jc_dict_names
  colnames(upload_dict_dpt_jc_home) <- jc_dict_names
  
  upload_dict_dpt_jc <- rbind(upload_dict_dpt_jc_wrk,
                              upload_dict_dpt_jc_home) %>%
    mutate(Job.Code.Desc = case_when(
      str_detect(Job.Code.Desc, "&") ~ 
        str_replace(Job.Code.Desc, "&", "AND"),
      TRUE ~ Job.Code.Desc)) %>%
    mutate(Job.Code.Desc =
             str_trim(str_sub(Job.Code.Desc, 1, 50))) %>%
    distinct(across(-Job.Code.Desc), .keep_all = TRUE)
  # the distinct across all columns except for the job description is
  # because there can be slight differences in job codes names.
  # we don't need to be too concerned if there are slight differences
  # (like spelling mistakes or a level of a position (e.g. coder 1 vs coder 2))
  # With this, the names in Premier may differ from what is in the Universal
  # mapping file
  
  # rm(upload_dict_dpt_jc_wrk, upload_dict_dpt_jc_home, jc_dict_names)
  
  # update dpt job code map
  upload_map_dpt_jc <- upload_dict_dpt_jc %>%
    select(-Job.Code.Desc) %>%
    mutate(Cost.Center =
             as.character(Cost.Center)) %>%
    left_join(map_premier_dpt %>%
                select(-Effective.Date)) %>%
    mutate(Cost.Center.Map = as.double(Cost.Center.Map)) %>%
    # if rbind() performed on the new upload_map_dpt and map_premier_dpt
    # then could join with that single unified data.frame instead of performing
    # a case_when for the Cost.Center.Map value
    mutate(Cost.Center.Map = case_when(
      is.na(Cost.Center.Map) ~ new_dpt_map,
      TRUE ~ Cost.Center.Map)) %>%
    left_join(map_uni_jobcodes_bislr %>%
                select(J.C.prem, PREMIER.J.C) %>%
                distinct(),
              by = c("Job.Code" = "J.C.prem")) %>%
    mutate(effective_date = format(map_effective_date, "%m/%d/%Y")) %>%
    relocate(effective_date, .before = Corporation.Code) %>%
    distinct()
  # new jobcodes that have not been updated in the universal mapping will
  # show up as NA.
  # We can either
  # (1) keep them and let them show up as upload errors
  # (2) have to discover them when publishing or by running the unmapped JC
  # report in Premier)
  # (3) manually correct them before uploading and update the Universal Mapping
  # file
  # FYI check:
  upload_map_dpt_jc_na <- upload_map_dpt_jc %>%
    filter(is.na(PREMIER.J.C))
  View(upload_map_dpt_jc_na)
  
  # the upload_map_dpt_jc file can be uploaded after all other files are
  # uploaded, so it could be manually handled and uploaded later after
  # the script has been run.
  
  # we can create separate sets of upload files? files that can be uploaded
  # cleanly and then other files that require manual input before upload
  
  # if we want to ensure that we do not have to run the code 2x to handle
  # new job codes, we can import the dept jc mapping file to compare
  # and identify what is new
  
  # are there jobcodes for the July data that show up in the missing_jc_map
  # below?
  
  # FYI check:
  missing_jc_map <- new_jobcodes %>%
    filter(!(Job.Code %in% upload_map_dpt_jc$Job.Code_up))
  View(missing_jc_map)
  # it's possible that the rows with the new jobcodes are in
  # time periods that won't be uploaded.
  # perhaps the new_jobcodes should be created after filtering the data
  # down to the date range of interest or the date range filtering should
  # be performed earlier
  # Though, it should be fine if there are extra mappings from old data
  # that were not previously mapped.
    
  
  # test data.frame for new paycodes
  new_paycodes <- bislr_payroll %>%
    select(Facility.Hospital.Id_Worked, Pay.Code) %>%
    unique() %>%
    tail()
  ###
  
  # alternatively, check for existence of file written the first time
  # through?
  # I think we need to walk through this process or process map it.
  if (exists("new_paycodes")) {
    
    upload_dict_paycode <- new_paycodes %>%
      left_join(
        select(map_uni_paycodes, RAW.PAY.CODE, PAY.CODE, PAY.CODE.NAME),
        by = c("Pay.Code" = "RAW.PAY.CODE")) %>%
      # Corporation.Code can come from somewhere else, but it doesn't
      # naturally come in from a join like with other dictionaries.
      # perhaps make it a constant
      mutate(Corporation.Code = 729805) %>% 
      rename(Site = Facility.Hospital.Id_Worked) %>%
      relocate(Corporation.Code, .before = Site) %>%
      mutate(Pay.Code = NULL)
    
    if (max(nchar(upload_dict_paycode$PAY.CODE)) > char_len_paycode |
        max(nchar(upload_dict_paycode$PAY.CODE.NAME)) > char_len_paycode_name){
      showDialog(title = "Paycode field error",
                 message = paste0("Either a paycode or paycode name has more ",
                                  "characters than permitted.  ",
                                  "Please fix them and rerun this segment of",
                                  "the script."))
      stop("Fix paycode errors and restart")
    }
    
    upload_map_paycode <- upload_dict_paycode %>%
      left_join(select(map_uni_paycodes, -RAW.PAY.CODE)) %>%
      mutate(PAY.CODE.NAME = NULL) %>%
      mutate(effective_date = format(map_effective_date, "%m/%d/%Y"),
             # should this percent be a constant set at beginning of script?
             allocation_pct = 100) %>%
      relocate(effective_date, .before = Corporation.Code)
      
    # this seems like a rare occurrence that we can handle manually.
    # MM: I think that if there are new paycodes, we can simply warn the user
    # of the issue, and require that the universal mapping, dictionary,
    # and mapping files all be taken care of before coming back to start running
    # the script from the beginning.  We could have the script stop at the
    # check performed in "Update Universal Files" section
    # MSHQ doesn't do special prep in the script - it's supposed to stop
    
    # the paycode dictionary seems challenging to automate
    # because we might have to take special consideration on the name of the
    # paycode if it's too many characters and gets shortened and becomes a
    # duplicate of one that already exists.
    
    # it seems like this part of the code would not be run if we identified
    # new pay codes because we would have manually updated them in the 
    # universal mapping file
    
    # if we updated the universal mapping file before this section of code, we
    # would then need to compare with the paycode dictionary and mapping files
    # downloaded from Premier in order to know what's not in Premier yet.
    
    # do we want to manually prepare the paycode dict and mapping files after
    # we update the universal mapping file?
    
    # should the effective date on this be different from the
    # map_effective_date? because Premier is sensitive to the effective date
    # and upload date of paycodes (we would have to upload payroll data in
    # history with this paycode in order for it to become effective)
    
    # FYI:
    # if there's a new paycode at one site, we should upload it for all sites
    
  }
  #dummy report upload

# Quality Checks -------------------------------------------------------


# Visualizations ----------------------------------------------------------


# Exporting Data ----------------------------------------------------------

  # remember to output Site Hours Quality Check

