dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity",
              "/Volume - Data/Multisite Volumes/Census Days")
dir_universal <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity",
                        "/Productivity/Universal Data")

# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(xlsx)
library(openxlsx)

# Constants ---------------------------------------------------------------
#Current names of sites - one for each site (order matters)
site_names <- c("MSB", "MSBI", "MSM", "MSW")
# Old site names - all old names and current name
# (order matters, must match above)
site_old_names <- list(c("BIB", "MSB"),
                       c("BIPTR", "MSBITR", "MSBI"),
                       c("STL", "MSM"),
                       c("RVT", "MSW"))

# Import Dictionaries -------------------------------------------------------
map_CC_Vol <- read.xlsx(paste0(dir,
                               "/BIBSLW_Volume ID_Cost Center_ Mapping.xlsx"))
dict_PC_raw <- read.xlsx(paste0(dir_universal, "/Mapping/MSHS_Pay_Cycle.xlsx"),
                     detectDates = T)
dict_PC <- dict_PC_raw %>% select(DATE, START.DATE, END.DATE) %>% drop_na()
colnames(dict_PC) <- c("Census.Date", "Start.Date", "End.Date")

map_reports <- read.xlsx(
  paste0(dir_universal, "/Mapping/MSHS_Reporting_Definition_Mapping.xlsx"),
  detectDates = T)


# Constants - Dates--------------------------------------------------------

#Table of distribution dates earlier than the current date
dist_dates <- dict_PC_raw %>%
  select(END.DATE, PREMIER.DISTRIBUTION) %>%
  distinct() %>%
  drop_na() %>%
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
answer <- winDialog(
  message = paste0(
    "Starting Date will be: ", pp.start, "\r\r",
    "Ending Date will be: ", pp.end, "\r\r",
    "If this is correct, press OK\r\r",
    "If this is not correct, press Cancel and\r",
    "you will be prompted to provide the correct\r",
    "date range."
  ),
  type = "okcancel"
)

if (answer == "CANCEL") {
  pp.start <- as.Date(
    rstudioapi::showPrompt(
      title = "pp.start input",
      message = paste0("What is the date you'd like the data to start from?\r",
                       "\rPlease enter the date in YYYY-MM-DD format")))
  pp.end <- as.Date(
    rstudioapi::showPrompt(
      title = "pp.start input",
      message = paste0("What is the date you'd like the data to go up to?\r",
                       "\rPlease enter the date in YYYY-MM-DD format")))
}

# Import Data -------------------------------------------------------------
import_recent_file <- function(folder.path, place) {
  #Importing File information from Folder
  File.Name <- list.files(path = folder.path, pattern = "xlsx$", full.names = F)
  File.Path <- list.files(path = folder.path, pattern = "xlsx$", full.names = T)
  File.Date <- as.Date(
    sapply(File.Name,
           function(x) substr(x, nchar(x) - 12, nchar(x) - 5)),
    format = "%m.%d.%y")
  File.Table <<- data.table::data.table(File.Name, File.Date, File.Path) %>%
    arrange(desc(File.Date))
  #Importing Data
  data_recent <- read.xlsx(File.Table$File.Path[place], detectDates = T)
  #File Source Column for Reference
  data_recent <- data_recent %>% mutate(Source = File.Table$File.Path[place])
  return(data_recent)
}
data_census <- import_recent_file(paste0(dir, "/Source Data"), 1)
#select which file you want instead of most recent file
# data_census <- read.xlsx(choose.files(caption = "Select Census File",
#                                       multi = F,
#                                       default = paste0(dir, "/Source Data")),
#                          detectDate = T)

create_rds <- function(x) {
  file_data <- lapply(File.Table$File.Path,
                      function(y) read.xlsx(y, detectDates = T))
  # table_data <- cbind(File.Table,
  #                     sapply(file_data, function(z) range(z$Census.Date)))
  date_range <- sapply(file_data, function(z) range(z$Census.Date))


}

# QC ----------------------------------------------------------------------
#Checking range of dates requested exists in the census file
if (pp.end > range(data_census$Census.Date)[2] |
    pp.start < range(data_census$Census.Date)[1]) {
  if (format(pp.end, "%Y") > format(pp.start, "%Y") &
      pp.end > range(data_census$Census.Date)[2]) {
    #If the end date requested goes beyond the calendar year of the census file
    warning(paste0("File only has calendar year to date data, ",
                   "end date will be changed to ",
                   format(pp.start, "%Y"), "-12-31"))
    pp.end.new <- as.Date(paste0(format(pp.start, "%Y"), "-12-31"))
  }else if (format(pp.start, "%Y") < format(pp.end, "%Y") &
            pp.start < range(data_census$Census.Date)[1]) {
    #If the start date requested is before the calendar year of the census file
    warning(paste0("File only has calendar year to date data, ",
                   "start date will be changed to ",
                   format(pp.end, "%Y"), "-01-01"))
    pp.start.new <- as.Date(paste0(format(pp.end, "%Y"), "-01-01"))
  }else{#If date range requested is outside the range of the census file
    stop(paste0("Data Missing from Census file for Pay Periods Needed. ",
                "Please add most recent file to the source data folder."))
  }
}

#Checking if any sites have been renamed in Census file
if (any(!c(as.vector(unique(data_census$Site), mode = "any")) %in%
        c(site_names, unlist(site_old_names)))) {
  new_site_names <- which(!c(as.vector(unique(data_census$Site),
                                       mode = "any")) %in%
                            c(site_names, unlist(site_old_names)))
  new_site_names <- c(as.vector(unique(data_census$Site),
                                mode = "any"))[new_site_names]
  warning("New site name(s) found: ", paste(new_site_names, collapse = ", "))
  stop(paste0("Please update the new site names in the constants section",
              "and rerun code"))
}

#Checking the pay cycle dictionary is up to date
if (range(data_census$Census.Date)[2] > range(dict_PC$End.Date)[2]) {
  stop("Update Pay Cycle Dictionary")
  }

# Pre Processing ----------------------------------------------------------
data_census <- data_census %>%
  mutate(Site = as.character(Site),
         Nursing.Station.Code = as.character(Nursing.Station.Code))
map_CC_Vol <- map_CC_Vol %>%
  select(Site, Nursing.Station.Code, CostCenter, VolumeID) %>%
  mutate(Nursing.Station.Code = as.character(Nursing.Station.Code),
         CostCenter = as.character(CostCenter),
         VolumeID = as.character(VolumeID)) %>%
  drop_na() %>%
  distinct()
# if any of the files have old site names update them to new site names
if (any(!unique(map_CC_Vol$Site) %in% site_names) |
    any(!unique(data_census$Site) %in% site_names)) {
  for (i in seq_len(length(site_names))) {
    data_census$Site <- gsub(paste(unlist(site_old_names[i]), collapse = "|"),
                             site_names[i], data_census$Site)
    map_CC_Vol$Site <- gsub(paste(unlist(site_old_names[i]), collapse = "|"),
                            site_names[i], map_CC_Vol$Site)
  }
}

map_reports <- map_reports %>%
  filter(CLOSED > pp.end | is.na(CLOSED)) %>%
  select(ORACLE.COST.CENTER, DEFINITION.CODE, DEFINITION.NAME) %>%
  distinct() %>%
  drop_na() %>%
  rename(CostCenter = ORACLE.COST.CENTER,
         ReportName = DEFINITION.NAME,
         ReportCode = DEFINITION.CODE)

data_upload <- left_join(data_census, map_CC_Vol)
data_upload <- left_join(data_upload, dict_PC)
#data_upload <- left_join(data_upload, map_reports)

# QC --------------------------------------------------------------------
#checking to see if vlookups are duplicating rows
if (nrow(data_census) != nrow(data_upload)) {
  stop("Check Dictionaries for Duplicates")
  }

# Upload File Creation ----------------------------------------------------
upload_file <- function(site.census, site.premier, map_cc) {
  upload <- data_upload %>%
    as.data.frame() %>%
    filter(Census.Date >= pp.start,
           Census.Date <= pp.end,
           Site %in% site.census) %>%
    mutate(Corp = 729805,
           Site = site.premier,
           Start.Date = format(Start.Date, "%m/%d/%Y"),
           End.Date = format(End.Date, "%m/%d/%Y")) %>%
    select(Corp, Site, CostCenter, Start.Date, End.Date,
           VolumeID, Census.Day) %>%
    group_by(Corp, Site, CostCenter, Start.Date, End.Date, VolumeID) %>%
    summarise(Volume = sum(Census.Day, na.rm = T)) %>%
    mutate(Budget = 0)
  upload <- na.omit(upload)
  #Adding Zeros for nursing stations with no census
  payperiods <- upload %>%
    ungroup() %>%
    select(Start.Date, End.Date) %>%
    distinct()
  map_cc <- map_cc %>%
    filter(Site %in% site.census) %>%
    select(CostCenter, VolumeID) %>%
    distinct()
  map_cc <- merge(map_cc, payperiods)
  map_cc <- map_cc %>% mutate(Concat = paste0(VolumeID, Start.Date))
  upload <- upload %>% mutate(Concat = paste0(VolumeID, Start.Date))
  zero_rows <- map_cc[!(map_cc$Concat %in% upload$Concat), ]
  zero_rows <- zero_rows %>%
    mutate(Corp = 729805, Site = site.premier, Volume = 0, Budget = 0)
  upload <- plyr::rbind.fill(upload, zero_rows)
  upload$Concat <- NULL
  return(upload)
}
#If there is a payperiod that goes into a different calendar year
new_start_end <- function(upload_file) {
  if (exists("pp.end.new")) {
    upload_file$End.Date <- gsub(format(pp.end, "%m/%d/%Y"),
                                 format(pp.end.new, "%m/%d/%Y"),
                                 upload_file$End.Date)
    return(upload_file)
  }else if (exists("pp.start.new")) {
    upload_file$Start.Date <- gsub(format(pp.start, "%m/%d/%Y"),
                                   format(pp.start.new, "%m/%d/%Y"),
                                   upload_file$Start.Date)
    return(upload_file)
  }else{
    return(upload_file)
  }
}
#Creating upload file for each site
# data_upload_MSW <- new_start_end(upload_file(site_names[4],
#                                              'NY2162', map_CC_Vol))
data_upload_MSMW <- new_start_end(
  rbind(upload_file(site_names[4], "NY2162", map_CC_Vol),
        upload_file(site_names[3], "NY2163", map_CC_Vol)))
data_upload_MSBIB <- new_start_end(
  rbind(upload_file(site_names[1], "630571", map_CC_Vol),
        upload_file(site_names[2], "630571", map_CC_Vol)))

## Premier 2.0 Headers ------------------------------------------------------

upload_cols <- c("Corporation Code",
                 "Entity Code",
                 "Cost Center Code",
                 "Start Date",
                 "End Date",
                 "Volume Code",
                 "Actual Volume",
                 "Budget Volume")

colnames(data_upload_MSMW) <- upload_cols
colnames(data_upload_MSBIB) <- upload_cols

# Export Files ------------------------------------------------------------
write.table(data_upload_MSMW,
            file = paste0(dir, "/Upload Files", "/MSMW_Census Days_",
                          if (exists("pp.start.new")) {
                            format(pp.start.new, "%Y-%m-%d")
                          }else{
                            format(pp.start, "%Y-%m-%d")
                          },
                          " to ",
                          if (exists("pp.end.new")) {
                            format(pp.end.new, "%Y-%m-%d")
                          }else{
                            format(pp.end, "%Y-%m-%d")
                          },
                          ".csv"),
            sep = ",", row.names = F, col.names = T)
write.table(data_upload_MSBIB,
            file = paste0(dir, "/Upload Files", "/MSBIB_Census Days_",
                          if (exists("pp.start.new")) {
                            format(pp.start.new, "%Y-%m-%d")
                          }else{
                            format(pp.start, "%Y-%m-%d")
                          },
                          " to ",
                          if (exists("pp.end.new")) {
                            format(pp.end.new, "%Y-%m-%d")
                          }else{
                            format(pp.end, "%Y-%m-%d")
                          },
                          ".csv"),
            sep = ",", row.names = F, col.names = T)

# Generating Quality Chart ------------------------------------------------
quality_chart <- function(data, site.census) {
  data_chart <- data_upload %>%
    ungroup() %>%
    filter(Site == site.census) %>%
    select(#ReportCode,
           #ReportName,
           CostCenter,
           Nursing.Station.Code,
           End.Date,
           Census.Day) %>%
    arrange(Nursing.Station.Code, End.Date) %>%
    mutate(End.Date = format(End.Date, "%Y-%m-%d")) %>%
    pivot_wider(names_from = End.Date, values_from = Census.Day,
                values_fn = list(Census.Day = sum)) #%>%
    #mutate(Report = paste(ReportCode, ReportName, sep = "-")) %>%
    #arrange(Report)
}
chart_master <- lapply(as.list(unique(data_upload$Site)),
                       function(x) quality_chart(data_upload, x))

# Export Quality Charts ---------------------------------------------------
write.xlsx2(chart_master[1],
            file = paste0(dir,
                          "/Quality Chart_",
                          format(Sys.time(), "%Y-%m-%d"),
                          ".xlsx"),
            row.names = F,
            sheetName = unique(data_upload$Site)[1])
sapply(2:length(chart_master),
       function(x) write.xlsx2(chart_master[x],
                               file = paste0(dir,
                                             "/Quality Chart_",
                                             format(Sys.time(), "%Y-%m-%d"),
                                             ".xlsx"),
                               row.names = F,
                               sheetName = unique(data_upload$Site)[x],
                               append = T))
