
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(xlsx)

# Directories -------------------------------------------------------------
dir <- 'J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity'
dir_BISLR <- paste0(dir, '/Labor - Data/Multi-site/BISLR')
dir_universal <- paste0(dir, '/Universal Data')
  
# Constants ---------------------------------------------------------------
new_dpt_map <- 10095
map_effective_date <- as.Date('2022-01-01') #is this date ok?
accural_legacy_cc <- c(1109008600, 1109028600, 4409008600, 6409008600) #add other 8600, make quality check for new 8600, id errors non accural oracle but backmapped accural
productive_paycodes <- c('REGULAR', 'OVERTIME', 'EDUCATION', 'ORIENTATION',
                        'OTHER_WORKED', 'AGENCY')
# general improvement opportunity:
# can we update the paycode mapping file to indicate productive vs. non-prod?

  ## Premier Formatting ------------------------------------------------------
  char_len_dpt <- 15
  char_len_dpt_name <- 50
  char_len_employ <- 30
  char_len_jobcode <- 10
  char_len_paycode <- 15

# Functions --------------------------------------------------------------
  import_recent_file <- function(folder.path, place) {
  #Importing File information from Folder
  File.Name <- list.files(path = folder.path, full.names = F)
  File.Path <- list.files(path = folder.path, full.names = T)
  File.Date <- as.Date(sapply(File.Name, function(x) substr(x,nchar(x)-14, nchar(x)-5)),
                       format = '%m_%d_%Y')
  File.Table <<- data.table::data.table(File.Name, File.Date, File.Path) %>%
    arrange(desc(File.Date))
  #Importing Data 
  data_recent <- read.table(File.Table$File.Path[place],
                            header = T,
                            as.is = T,
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
bislr_payroll <- import_recent_file(paste0(dir_BISLR, '/Source Data'), 1)
  #quality check if correct file selected


## Wide Pivot Check -------------------------------------------------------

# the imported data having an unresolved issue leads to this table having
# some incorrect values
  
# will date ranges need to be confirmed by the user earlier?
# or do we just look at all dates in the data?
dist_prev <- as.Date("2022-07-02")
dist_current <- as.Date("2022-07-30")

piv_wide_check <- bislr_payroll %>%
  filter(as.Date(End.Date, "%m/%d/%Y") >= dist_prev &
           as.Date(End.Date, "%m/%d/%Y") <= dist_current + 7) %>%
  group_by(Facility.Hospital.Id_Worked, Payroll.Name, End.Date) %>%
  summarize(Hours = sum(as.numeric(Hours), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(as.Date(End.Date, "%m/%d/%Y"),
          Facility.Hospital.Id_Worked, Payroll.Name) %>%
  pivot_wider(names_from = End.Date,
             values_from = Hours)

piv_wide_check2 <- bislr_payroll %>%
  filter(as.Date(End.Date, "%m/%d/%Y") > dist_prev - 1 &
           as.Date(End.Date, "%m/%d/%Y") < dist_current + 8) %>%
  group_by(Facility.Hospital.Id_Worked, End.Date) %>%
  summarize(Hours = sum(as.numeric(Hours), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(as.Date(End.Date, "%m/%d/%Y"),
          Facility.Hospital.Id_Worked) %>%
  pivot_wider(names_from = End.Date,
              values_from = Hours)

piv_wide_check3 <- bind_rows(piv_wide_check, piv_wide_check2) %>%
  mutate(Payroll.Name = case_when(
    is.na(Payroll.Name) ~ "-SITE TOTAL-",
    TRUE ~ Payroll.Name)) %>%
  arrange(Facility.Hospital.Id_Worked, Payroll.Name)
rm(piv_wide_check2)

View(piv_wide_check)

# Import References -------------------------------------------------------
#delete start.end in mapping file and create in script to identify which paycycles to filter on in payroll files
  pay_cycles_uploaded <- read.xlsx(paste0(dir_BISLR,
                                        "/Reference",
                                        "/Pay cycles uploaded_Tracker.xlsx"),
                                 detectDates = T,
                                 sheetIndex = 1)
msus_removal_list <- read_xlsx(paste0(dir_BISLR,
                                      "/Reference/MSUS_removal_list.xlsx"),
                               sheet = 1)
#TBD references continued

# Data Processing -----------------------------------------------------------


# Creating Outputs --------------------------------------------------------


# Quality Checks -------------------------------------------------------


# Visualizations ----------------------------------------------------------


# Exporting Data ----------------------------------------------------------



