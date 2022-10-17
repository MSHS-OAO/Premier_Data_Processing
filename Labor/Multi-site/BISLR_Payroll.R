
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
map_effective_date <- as.Date('2010-01-01')
# MM: can we just look at the 4-digit department and look for 8600?
# (e.g. look at the 4 right digits of the legacy cost cost center)
# or can we put these values from the department dictionary?
# having a check for new 8600 accrual depts is a good idea, but is
# it necessary?
# Or can we add these to the Universal Report Def file?
accural_legacy_cc <- c(1109008600, 1109028600, 4409008600, 6409008600) #add other 8600, make quality check for new 8600, id errors non accural oracle but backmapped accural
# MM: general improvement opportunity:
# can we update the paycode mapping file to indicate productive vs. non-prod?
productive_paycodes <- c('REGULAR', 'OVERTIME', 'EDUCATION', 'ORIENTATION',
                        'OTHER_WORKED', 'AGENCY')

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
  #departments coming in as numeric doesn't matter as we create
  #our own cost center column and do not use what is in the raw data anyway
  return(data_recent)
  }

# Import Data -------------------------------------------------------------
raw_payroll <- import_recent_file(paste0(dir_BISLR, '/Source Data'), 1)
#bislr_payroll <- import_recent_file(paste0(dir_BISLR, '/Source Data'), 1)
  #raw_payroll_export <- bislr_payroll

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
    mutate(J.C = str_trim(J.C)) %>%
    mutate(JC_in_UnivseralFile = 1)
  map_uni_paycodes <- map_uni_paycodes %>%
    mutate(Paycode_in_Universal = 1)
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
  dummy_report_list  <- do.call(rbind, dummy_report_list) %>%
    rename(Cost.Center = value) 

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
  bislr_payroll <- raw_payroll %>%
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
                                                 digits = 0),
           Job.Code = str_trim(Job.Code),
           Position.Code.Description = str_trim(Position.Code.Description)) %>%
    mutate(DPT.WRKD = case_when(
      trimws(Department.Name.Worked.Dept) == "" ~ as.character(Department.IdWHERE.Worked),
      TRUE ~ DPT.WRKD),
      DPT.HOME = case_when(
        trimws(Department.Name.Home.Dept) == "" ~ as.character(Department.ID.Home.Department),
        TRUE ~ DPT.HOME)) %>%
    mutate(DPT.WRKD = case_when(
      DPT.WRKD.LEGACY %in% accural_legacy_cc ~ DPT.WRKD.LEGACY,
      TRUE ~ DPT.WRKD),
      Department.Name.Worked.Dept = case_when(
        DPT.WRKD.LEGACY %in% accural_legacy_cc ~ "ACCRUAL COST CENTER",
        TRUE ~ Department.Name.Worked.Dept),
      Job.Code = case_when(
        paste0(DPT.WRKD, '-', Employee.Name) %in%
          paste0(msus_removal_list$`Department IdWHERE Worked`,
                 '-', msus_removal_list$`Employee Name`)
        ~ unique(msus_removal_list$`New Job Code`),
        TRUE ~ Job.Code)) %>%
    left_join(pay_cycles_uploaded) %>%
    left_join(map_uni_jobcodes %>% 
                filter(PAYROLL == 'BISLR') %>%
                select(J.C, PROVIDER, JC_in_UnivseralFile) %>%
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
    mutate(Job.Code_up = substr(Job.Code, 1, 10))

    ## Update Universal Files --------------------------------------------------
    # if (NA %in% unique(bislr_payroll$JC_in_UnivseralFile)) {
    #   new_jobcodes <- bislr_payroll %>%
    #     filter(is.na(JC_in_UnivseralFile)) %>%
    #     select(Job.Code, Position.Code.Description) %>%
    #     unique() %>%
    #     mutate(JobDescCap = toupper(Position.Code.Description)) %>%
    #     left_join(map_uni_jobcodes %>%
    #                 filter(PAYROLL == 'MSHQ') %>%
    #                 select(J.C.DESCRIPTION, PROVIDER, PREMIER.J.C,
    #                        PREMIER.J.C.DESCRIPTION) %>%
    #                 rename(JobDescCap = J.C.DESCRIPTION)) %>%
    #     select(-JobDescCap) %>%
    #     unique()
    #   View(new_jobcodes)
    #   write.csv(new_jobcodes, 'New Job Codes for Universal File.csv')
    #   
    # 
    #   # if not all have a recommendation from MSHQ, we could look in BISLR
    #   # because there are times when the name is the same but the jobcode is
    #   # different
    #   
    #   stop('New job codes detected, update universal job code dictionary before continuing to run code')
    # }
  bislr_payroll_test <- head(bislr_payroll)
  bislr_payroll_test$Job.Code[1] <- 'test_jc'
  bislr_payroll_test$JC_in_UnivseralFile[1] <- NA
    loop <- 0
    while (NA %in% unique(bislr_payroll_test$JC_in_UnivseralFile)) {
      loop <- loop + 1
      new_jobcodes <- bislr_payroll_test %>%
        filter(is.na(JC_in_UnivseralFile)) %>%
        select(Job.Code, Position.Code.Description) %>%
        unique() %>%
        mutate(JobDescCap = toupper(Position.Code.Description)) %>%
        left_join(map_uni_jobcodes %>%
                    filter(PAYROLL == 'MSHQ') %>%
                    select(J.C.DESCRIPTION, PROVIDER, PREMIER.J.C,
                           PREMIER.J.C.DESCRIPTION) %>%
                    rename(JobDescCap = J.C.DESCRIPTION)) %>%
        select(-JobDescCap) %>%
        unique()
      View(new_jobcodes)
      write.csv(new_jobcodes,
                paste0('New Job Codes for Universal File',
                       if(loop == 1){''}else{paste0('_V',loop)},
                       '.csv'))

      showQuestion(
        title = 'Warning',
        message = paste0(if(loop == 1){
          'New job codes detected! \n'}else{
            'There are still new job codes. \n'
            },
                         'Update Universal Job Code File before continuing. \n',
                         '\n Have new jobes been added?'),
                             ok = 'Yes',
                             cancel = 'No')
      
      map_uni_jobcodes <- read_xlsx(paste0(dir_universal,
                                           '/Mapping/MSHS_Jobcode_Mapping.xlsx'),
                                    sheet = 1)
      map_uni_jobcodes <- map_uni_jobcodes %>%
        mutate(J.C = str_trim(J.C)) %>%
        mutate(JC_in_UnivseralFile = 1)
      
      bislr_payroll_test <- left_join(bislr_payroll_test %>%
                                   select(-JC_in_UnivseralFile, - PROVIDER),
                                 map_uni_jobcodes %>% 
                                   filter(PAYROLL == 'BISLR') %>%
                                   select(J.C, PROVIDER, JC_in_UnivseralFile) %>%
                                   rename(Job.Code = J.C))
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
  
  #Paycycles to filter on
  filter_dates <- bislr_payroll %>%
    filter(is.na(Pay_Cycle_Uploaded)) %>%
    select(Start.Date, End.Date) %>%
    unique() %>%
    arrange(Start.Date) %>%
    filter(End.Date > dist_prev,
           !Start.Date > distribution_date) %>%
    mutate(upload_date = 1)
  
  # MM: is it worth filtering out dates that are not of interest at this point
  # in the script?
  # bislr_payroll <- bislr_payroll %>%
  #   left_join(filter_dates) %>%
  #   filter(!is.na(upload_date))

# Formatting Outputs ---------------------------------------------------------
  
  ## JC ID check ----------------------------------------------------
  # this section is here because if any job codes become duplicates after
  # getting shortened then we need to be aware of the conflict and resolve it
  # This might warrant a restructuring of the JC universal mapping file
  
  # There's an assumption that the universal jc mapping file has been updated
  # with new jobcodes by this point.
  
  #MM: update this to compare new shortened jobcode with the
  # dict_premier_jobcode
  
  # if new jobcode (based on JC_in_UnivseralFile)
  # and shortened
  # compare with the premier dictionary
  
  # This will get incorporated into Preprocessing-Updating Universal Files section
  
  # if (sum(long_jc$freq) > length(long_jc$freq)) {
  #   showDialog(title = "ERROR: Job Codes",
  #              message = paste0("There are duplicates in ",
  #                               "shortened Job Codes.  ",
  #                               "These will require special handling.  ",
  #                               "Please stop the script and resolve concerns ",
  #                               "before restarting.")
  #   )
  #   stop(paste0("There are duplicates in shortened Job Codes.\n\n",
  #               "These will require special handling.\n\n",
  #               "Please stop the script and resolve concerns ",
  #               "before restarting.\n"))
  # } else {
  #   cat("All shortened job codes are unique\n")
  # }
  
  
  ## Premier Payroll File ----------------------------------------------------
  
  upload_payroll <- bislr_payroll %>%
    # is there a method to filter on multiple columns instead of join?
    # reference the DUS_RMV mutate()-case_when() as an example
    # join seems simple/quick enough that we can keep it.
    left_join(filter_dates) %>%
    filter(!is.na(upload_date)) %>%
    # if any PROVIDER value is NA, then it needs to be mapped
    # it will be checked for in QC section
    filter(PROVIDER == 0) %>%
    group_by(
      PartnerOR.Health.System.ID,
      Home.FacilityOR.Hospital.ID, DPT.HOME,
      Facility.Hospital.Id_Worked, DPT.WRKD,
      Start.Date, End.Date,
      Employee.ID, Employee.Name,
      Approved.Hours.per.Pay.Period,
      Job.Code_up,
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
  # this will be split into BIB and SLW in the Exporting section

  ## Premier Reference Files -------------------------------------------------
  
  # update dpt dict

  # wrap the new dept in a an if looking at columns for NA
  
  upload_dict_dpt <- rbind(bislr_payroll %>%
                             filter(is.na(HomeDpt_in_Dict)) %>%
                             select(PartnerOR.Health.System.ID,
                                    Home.FacilityOR.Hospital.ID,
                                    DPT.HOME, Department.Name.Home.Dept) %>%
                             setNames(
                               colnames(dict_premier_dpt %>%
                                          select(-Dpt_in_Dict))),
                           bislr_payroll %>%
                             filter(is.na(WRKDpt_in_Dict)) %>%
                             select(PartnerOR.Health.System.ID,
                                    Facility.Hospital.Id_Worked,
                                    DPT.WRKD, Department.Name.Worked.Dept) %>%
                             setNames(
                               colnames(dict_premier_dpt %>%
                                          select(-Dpt_in_Dict)))
                           ) %>%
    distinct() %>%
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

  # there's some sort of error in the Cost.Center id column
  # these are values: --1--83-000 & --1--85-000
  # Anjelica will code to handle this issue further up in the script.
  
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
  
  upload_dict_dpt_jc <- rbind(bislr_payroll %>%
                                filter(is.na(WRKJC_in_Dict),
                                       PROVIDER == 0) %>%
                                select(PartnerOR.Health.System.ID,
                                       Facility.Hospital.Id_Worked,
                                       DPT.WRKD, Job.Code_up,
                                       Position.Code.Description) %>%
                                setNames(colnames(dict_premier_jobcode %>%
                                                    select(-JC_in_Dict))),
                              bislr_payroll %>%
                                filter(is.na(HOMEJC_in_Dict),
                                       PROVIDER == 0) %>%
                                select(PartnerOR.Health.System.ID,
                                       Home.FacilityOR.Hospital.ID,
                                       DPT.HOME, Job.Code_up,
                                       Position.Code.Description) %>%
                                setNames(colnames(dict_premier_jobcode %>%
                                                    select(-JC_in_Dict)))
                              ) %>%
    # mutate(Job.Code.Description = case_when(
    #   str_detect(Job.Code.Description, "&") ~
    #     str_replace(Job.Code.Description, "&", "AND"),
    #   TRUE ~ Job.Code.Description)) %>%
    mutate(Job.Code.Description =
             str_trim(str_sub(Job.Code.Description, 1, 50))) %>%
    distinct(across(-Job.Code.Description), .keep_all = TRUE)
  
  # update dpt job code map
  upload_map_dpt_jc <- upload_dict_dpt_jc %>%
    select(-Job.Code.Description) %>%
    # the map_premier_dpt Cost.Center column is character type
    mutate(Cost.Center =
             as.character(Cost.Center)) %>%
    left_join(map_premier_dpt %>%
                select(-Effective.Date)) %>%
    mutate(Cost.Center.Map = as.double(Cost.Center.Map)) %>%
    mutate(Cost.Center.Map = case_when(
      is.na(Cost.Center.Map) ~ new_dpt_map,
      TRUE ~ Cost.Center.Map)) %>%
    # the map_uni_jobcodes_bislr data.frame should be refreshed by the time this
    # part of the code is run
    left_join(map_uni_jobcodes %>%
                filter(PAYROLL == 'BISLR') %>%
                select(J.C.prem, PREMIER.J.C),
              by = c("Job.Code" = "J.C.prem")) %>% #J.C.prem column doesn't exist anymore
    mutate(effective_date = format(map_effective_date, "%m/%d/%Y")) %>%
    relocate(effective_date, .before = Corporation.Code) %>%
    distinct()
  # when running the code live, new jobcodes that have not been updated in the
  # universal mapping should show up as NA.
  # but they should have been manually corrected by this point.
  
  # FYI check:
  # This can be moved into the QC section
  upload_map_dpt_jc_na <- upload_map_dpt_jc %>%
    filter(is.na(PREMIER.J.C))
  View(upload_map_dpt_jc_na)

  # FYI check:
  missing_jc_map <- new_jobcodes %>%
    filter(!(Job.Code %in% upload_map_dpt_jc$Job.Code_up))
  View(missing_jc_map)
  # perhaps the new_jobcodes should be created after filtering the data
  # down to the date range of interest or the date range filtering should
  # be performed earlier

  # test data.frame for new paycodes
  # new_paycodes <- bislr_payroll %>%
  #   select(Facility.Hospital.Id_Worked, Pay.Code) %>%
  #   unique() %>%
  #   tail()
  ###
  
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
        max(nchar(upload_dict_paycode$PAY.CODE.NAME)) > char_len_paycode_name) {
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
      # effective date should be the beginning of data file to be uploaded
      mutate(effective_date =
               format(min(upload_payroll$Start.Date), "%m/%d/%Y"),
             # should this percent be a constant set at beginning of script?
             allocation_pct = 100) %>%
      relocate(effective_date, .before = Corporation.Code)

    # FYI:
    # if there's a new paycode at one site, we should upload it for all sites

  }
  
  #dummy report upload
  upload_report_dict <- bislr_payroll %>%
    select(Home.FacilityOR.Hospital.ID,
           DPT.HOME,
           DPT.WRKD) %>%
    left_join(map_uni_reports %>%
                select(ORACLE.COST.CENTER) %>%
                rename(DPT.WRKD = ORACLE.COST.CENTER) %>%
                mutate(WRKD.DPT.in.Report = 1)) %>%
    left_join(map_uni_reports %>%
                select(ORACLE.COST.CENTER) %>%
                rename(DPT.HOME = ORACLE.COST.CENTER) %>%
                mutate(HOME.DPT.in.Report = 1)) %>%
    filter(WRKD.DPT.in.Report == 1,
           is.na(HOME.DPT.in.Report)) %>%
    select(-DPT.WRKD, -WRKD.DPT.in.Report, -HOME.DPT.in.Report) %>%
    rename(Site = Home.FacilityOR.Hospital.ID,
           Cost.Center = DPT.HOME)  %>%
    rbind(dummy_report_list %>%
            select(Site, Cost.Center)) %>%
      unique() %>%
      group_by(Site) %>%
    summarize(Cost.Center = paste(Cost.Center, collapse = ':')) %>%
    left_join(dummy_reports %>% select(-contains('blank'), -Cost.Center)) %>%
    relocate(Site, .after = Corporation.Code) %>%
    relocate(Cost.Center, .after = Report.ID)

# Quality Checks -------------------------------------------------------


# Visualizations ----------------------------------------------------------


# Exporting Data ----------------------------------------------------------

  # remember to output Site Hours Quality Check
