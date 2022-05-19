
# Load Libraries ----------------------------------------------------------
library(tidyverse)
#library(xlsx)
library(readxl)

# User Input --------------------------------------------------------------
#end date of the last pay period of data to refresh
pp_end_date <- as.Date('2022-01-01')

# Constants ---------------------------------------------------------------
#number of pay periods to refresh - standard is last 6 months or about 12 pay periods
num_payperiods_refresh <- 12

dir_rds <- paste0('J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity',
                  '/Volume - Data/MSLW Data/Both Sites Data/Charge Detail')
dir_universal <- paste0('J:/deans/Presidents/SixSigma/MSHS Productivity',
                        '/Productivity/Universal Data')

# Load Data ---------------------------------------------------------------
dictionary_pc <- read_xlsx(paste0(dir_universal,
                                  '/Mapping/MSHS_Pay_Cycle.xlsx'))
cpt_data <- readRDS(paste0(dir_rds,
                           '/Source Data/',
                           list.files(paste0(dir_rds, '/Source Data'),
                                      pattern = 'RDS$')))

# Preprocessing Data ------------------------------------------------------
#list of all standard biweekly pay period end dates
pp_refresh <- dictionary_pc %>%
  drop_na() %>%
  arrange(desc(DATE)) %>%
  filter(END.DATE <= pp_end_date) %>%
  select(END.DATE) %>%
  distinct()
#list of the last six months of pay periods to refresh
dates_refresh <- dictionary_pc %>%
  filter(END.DATE %in% pp_refresh$END.DATE[1:num_payperiods_refresh])

# Creating File -----------------------------------------------------------
file_upload <- function(all_data, site, start_date, end_date) {
  data_upload <- all_data %>%
    filter(ServiceDate >= start_date,
           ServiceDate <= end_date,
           Premier.Facility.ID == site) %>%
    mutate(Corp = '729805',
           EndDate = ServiceDate,
           ServiceDate = format(ServiceDate, '%m/%d/%Y'),
           EndDate = format(EndDate, '%m/%d/%Y')) %>%
    select(Corp, Premier.Facility.ID, Cost.Center, ServiceDate, EndDate,
           CPTCode, NumberOfUnits) %>%
    group_by(Corp, Premier.Facility.ID, Cost.Center, ServiceDate, EndDate,
             CPTCode) %>%
    summarise(Volume = sum(NumberOfUnits, na.rm = T)) %>%
    mutate(Budget = 0) %>%
    drop_na()
}

msw_upload <- file_upload(cpt_data, 'NY2162',
                          range(dates_refresh$DATE)[1],
                          range(dates_refresh$DATE)[2])
msm_upload <- file_upload(cpt_data, 'NY2163',
                          range(dates_refresh$DATE)[1],
                          range(dates_refresh$DATE)[2])
msmw_upload <- rbind(msw_upload, msm_upload)

# Export Data -------------------------------------------------------------
write.table(msmw_upload,
            file = paste0(dir_rds, '/MSMW_CPT_',
                          format(as.Date(range(msw_upload$ServiceDate)[1],
                                         format = '%m/%d/%Y'), '%d%b%y'),
                          ' to ',
                          format(as.Date(range(msw_upload$ServiceDate)[2],
                                         format = '%m/%d/%Y'), '%d%b%y'),
                          '.csv'),
            sep = ',', row.names = F, col.names = F)