# Libraries ---------------------------------------------------------------

library(dplyr)
library(data.table)
library(xlsx)
library(readxl)
library(tidyr)
library(lubridate)
library(stringr)
# does sequence of loading affect which functions are masked?

# Assigning Directory(ies) ------------------------------------------------

if ("Presidents" %in% list.files("J:/")) {
  j_drive <- "J:/Presidents"
} else {
  j_drive <- "J:/deans/Presidents"
}

dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity",
              "/Volume - Data/Multisite Volumes/Census Days")
dir_universal <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity",
                        "/Productivity/Universal Data")

# Data Import -------------------------------------------------------------

# select where the monthly data is pulled from
# this is typically on a hard-drive to expedite the import time
# but default is set to the shared drive location
raw_path <- choose.dir(
  default = paste0(j_drive, "/SixSigma/MSHS Productivity/Productivity",
                   "/Volume - Data/Multisite Volumes/Census Days"),
  caption = "Select folder above all data files")

# get file paths
all_folders <-
  list.dirs(
    path = raw_path,
    full.names = TRUE
  )

all_paths <-
  list.files(
    path = all_folders,
    pattern = "*.xlsx",
    full.names = TRUE
  )

# just file names
all_filenames <- all_paths %>%
  basename() %>%
  as.data.frame()
colnames(all_filenames) <- c("file")


# read file content
all_content <-
  all_paths %>%
  lapply(read.xlsx#,
         # sheetIndex = 1
         # header = TRUE
         # sep = ",",
         # encoding = "UTF-8",
         # stringsAsFactors = F
  )


# combine file content list and file name list
all_filenames <- all_paths %>%
  basename() %>%
  as.list()

# append file name to each data point
all_lists <- mapply(c, all_content, all_filenames, SIMPLIFY = FALSE)

# unlist all lists and change column name
raw_data <- rbindlist(all_lists, fill = T)
# change column name
raw_data <- raw_data %>%
  rename(file_path = V1)


# Reference Import --------------------------------------------------------

ref_path <- rstudioapi::selectFile(
  path = paste0(j_drive, "/SixSigma/MSHS Productivity/Productivity",
                   "/Volume - Data/Multisite Volumes/Census Days"),
  caption = "Select mapping file")

# This is the mapping file location for now:
# "J:\deans\Presidents\SixSigma\MSHS Productivity\Productivity\Volume - Data\Multisite Volumes\Census Days\MSMW temp alternative 2023-09\Department Mapping.xlsx"
# this can be moved as needed.


mapping <- read_xlsx(ref_path)

dict_PC_raw <- read.xlsx(paste0(dir_universal, "/Mapping/MSHS_Pay_Cycle.xlsx"),
                         detectDates = T)
dict_PC <- dict_PC_raw %>% select(DATE, START.DATE, END.DATE) %>% drop_na()
colnames(dict_PC) <- c("Census.Date", "Start.Date", "End.Date")

# Pre-processing ----------------------------------------------------------

processed_data <- raw_data %>%
  left_join(mapping %>%
              select(`ADT Mapping`, Entity, CostCenter, VolumeID),
            by = c("DEPARTMENT_NAME" = "ADT Mapping")) %>%
  filter(Entity %in% c("NY2163", "NY2162")) %>%
  mutate(Census.Date = as.Date(paste0(substr(YEAR_MONTH, 1, 4),
                               "-",
                               substr(YEAR_MONTH, 5, 6),
                               "-",
                               as.character(DAY_OF_MONTH)),
                        format = "%Y-%m-%d")) %>%
  left_join(dict_PC,
            by = c("Census.Date" = "Census.Date"))


upload <- processed_data %>%
  mutate(Corp = 729805,
         Start.Date = format(Start.Date, "%m/%d/%Y"),
         End.Date = format(End.Date, "%m/%d/%Y")) %>%
  select(Corp, Entity, CostCenter, Start.Date, End.Date,
         VolumeID, OCCUPIED_COUNT) %>%
  group_by(Corp, Entity, CostCenter, Start.Date, End.Date, VolumeID) %>%
  summarise(Volume = sum(OCCUPIED_COUNT, na.rm = T)) %>%
  mutate(Budget = 0)
upload <- na.omit(upload)

#Adding Zeros for nursing stations with no census
payperiods <- upload %>%
  ungroup() %>%
  select(Start.Date, End.Date) %>%
  distinct()

View(payperiods)
# In September 2023, there was 1 day present in the next pay period,
# so be aware of the need to remove the extra pay period.
rstudioapi::showDialog(title = "Pay Period Check",
                       message = paste0("Confirm what Pay Periods are ",
                                        "present.  If there are extra pay ",
                                        "periods then be sure to filter them,",
                                        "out."))

map_cc <- mapping %>%
  filter(Entity %in% c("NY2163", "NY2162")) %>%
  select(Entity, CostCenter, VolumeID) %>%
  distinct()
map_cc <- merge(map_cc, payperiods)
map_cc <- map_cc %>% mutate(Concat = paste0(VolumeID, Start.Date))
upload <- upload %>% mutate(Concat = paste0(VolumeID, Start.Date))
zero_rows <- map_cc[!(map_cc$Concat %in% upload$Concat), ]
zero_rows <- zero_rows %>%
  mutate(Corp = 729805, Volume = 0, Budget = 0) %>%
  relocate(Corp, .before = Entity)
upload <- plyr::rbind.fill(upload, zero_rows)
upload$Concat <- NULL
upload2 <- upload %>%
  distinct()

# Premier 2.0 Headers ------------------------------------------------------

upload_cols <- c("Corporation Code",
                 "Entity Code",
                 "Cost Center Code",
                 "Start Date",
                 "End Date",
                 "Volume Code",
                 "Actual Volume",
                 "Budget Volume")

colnames(upload) <- upload_cols

# Output ------------------------------------------------------------------

write_path <- choose.dir(default = raw_path,
                         caption = "Select folder to store upload file"
)

# had trouble getting dates in file name to work, so 
write.table(upload,
            file = paste0(write_path, "/MSMW_Census Days_temp",
                          ".csv"),
            sep = ",", row.names = F, col.names = T)

