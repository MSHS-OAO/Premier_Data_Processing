# Load Libraries -----------------------------------------------------------
library(readxl)
library(tidyverse)
library(openxlsx)

# Constants ----------------------------------------------------------------
dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity",
              "/Volume - Data/Multisite Volumes/CSPD")

#User inputs start and end dates for current period
start_date <- as.Date("2022-05-22", format = "%Y-%m-%d")
end_date <- as.Date("2022-07-02", format = "%Y-%m-%d")

# Load Data & Dictionaries -------------------------------------------------
#Pull in mapping file
volume_mapping <- read_excel(paste0(dir,
                                "/MSHS_Cost Center & Vol ID Mapping File.xlsx"),
                             col_types = c(rep("text", 7)))

#Preprocessing-------------------------------------------------------------
#Assigning sheet to the names of the excel sheets
sheet <- excel_sheets(paste0(dir, "/Source Data/",
                          "MSHS Central Sterile Volume Template Updated.xlsx"))

CSPDdf <- lapply(sheet,
                 function(x) {
                   read_excel(
                     paste0(dir, "/Source Data/",
                          "MSHS Central Sterile Volume Template Updated.xlsx"),
                     sheet = x,
                     col_names = TRUE)})

CSPDdf <- bind_rows(CSPDdf, .id = "Sheet") %>% #Combine sheets into one

select(3:11) #Select necessary columns

colnames(CSPDdf) <- CSPDdf[1, ]
CSPDdf <- CSPDdf[-1, ] #Make first row into column headers

#Replace Decontam Totals column with values from Assembly Totals
CSPDdf$`Decontam Totals` <- CSPDdf$`Assembly Totals`

CSPDdf <- CSPDdf %>%
  filter(`Volume ID` != "Volume ID")

#Filters out rows that repeat column headers and adds department ID for Queens
CSPDdf <- CSPDdf %>%
  filter(!is.na(`Decontam Totals`),
         !is.na(`Assembly Totals`),
         !is.na(`Sterilizer Totals`),
         !is.na(`Inventory Reporting (Send) Totals`)) %>%
  mutate(`Facility or Hospital ID` = case_when(
    is.na(`Facility or Hospital ID`) ~ "NY0014",
    TRUE ~ `Facility or Hospital ID`)) %>%
  mutate(`Department ID` = case_when(
    is.na(`Department ID`) ~ "102000010760170",
    `Department ID` == "01040181" ~ "101000010160170",
    TRUE ~ `Department ID`))

#Convert volume columns to numeric
CSPDdf[, c(6:9)] <- sapply(CSPDdf[, c(6:9)], as.numeric)

#Formats dates
CSPDdf <- CSPDdf %>%
  mutate(`Start Date` = convertToDate(`Start Date`),
         `End Date` = convertToDate(`End Date`)) %>%
  mutate(`Start Date` = paste0(substr(`Start Date`, 6, 7), "/",
                               substr(`Start Date`, 9, 10), "/",
                               substr(`Start Date`, 1, 4)),
         `End Date` = paste0(substr(`End Date`, 6, 7), "/",
                             substr(`End Date`, 9, 10), "/",
                             substr(`End Date`, 1, 4))) %>%
  mutate(`Start Date` = as.Date(`Start Date`,  format = "%m/%d/%Y"),
         `End Date` = as.Date(`End Date`, format = "%m/%d/%Y"))


#Sums up volume columns into one
CSPDdf <- CSPDdf %>%
  rowwise() %>%
  mutate(Volume = round(sum(c_across(`Assembly Totals`:
                                       `Inventory Reporting (Send) Totals`)),
                        digits = 2))

#Adds necessary columns and creates table
CSPDdf1 <- CSPDdf %>%
  as.data.frame() %>%
  mutate(Health_System_ID = "729805",
         Budget = "0") %>%
  filter(`Start Date` >= start_date, `End Date` <= end_date) %>%
  left_join(volume_mapping,
            by = c("Department ID" = "Old Cost Center")) %>%
  select(Health_System_ID, `Facility or Hospital ID`, `Department ID`,
         `Start Date`, `End Date`, `Vol ID`, Volume, Budget) %>%
  mutate(`Start Date` = as.character(`Start Date`, format = "%m/%d/%Y"),
         `End Date` = as.character(`End Date`, format = "%m/%d/%Y")) %>%
  `colnames<-`(c("Corporation Code", "Entity Code", "Cost Center Code", 
                 "Start Date", "End Date", "Volume Code", "Actual Volume",
                 "Budget Volume"))

#DATA REPOSITORY---------------------------------------------------------
#1 read master
old_master <- readRDS(paste0(dir, "/Master/Master.rds"))

#2 append master (2-3 pay periods of data)
new_master <- rbind(old_master, CSPDdf1)
  saveRDS(new_master, paste0(dir, "/Master/Master.rds"))

#3 validation
validation <- CSPDdf1 %>%
  pivot_wider(id_cols = `Department ID`, names_from = `End Date`,
              values_from = Volume)

#4 Validation and upload
write.table(validation, paste0(dir, "/CSPD Validation.csv"),
            row.names = F,
            col.names = T,
            sep = ",")

write.table(CSPDdf1, paste0(dir, "/Multisite_CSPD Volumes_.csv"),
            row.names = F,
            col.names = T,
            sep = ",")
