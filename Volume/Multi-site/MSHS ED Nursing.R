# Load Libraries -----------------------------------------------------------
library(readxl)
library(tidyverse)
library(openxlsx)
library(rstudioapi)
library(DBI)
library(odbc)
library(lubridate)
library(ggplot2)
library(dplyr)

# Assigning Directory(ies) ------------------------------------------------
dir <- paste0("/SharedDrive/deans/Presidents/SixSigma/MSHS Productivity/Productivity",
              "/Volume - Data/Multisite Volumes/ED Nursing")

showDialog(
  title = "Data Import Reminder",
  message = paste0("Make sure all necessary data is included ",
                   "in the raw data extracts. ",
                   "Multiple months data may be needed")
)
# Data Import -------------------------------------------------------------
#Raw ED Data
ED_visit_data_raw <- read.csv(paste0(dir,
                                     "/Raw Data/ED Visits Data.csv"))

ED_boarder_data_raw <- read.csv(paste0(dir,
                                       "/Raw Data/ED Boarder Visits Data.csv"))

# Data References ---------------------------------------------------------
#Pay Cycle from DB
oao_con <- dbConnect(odbc(), "OAO Cloud DB Production")
dict_pay_cycles <- tbl(oao_con, "LPM_MAPPING_PAYCYCLE") %>% collect()

mapping <- read_excel(paste0(dir,
                             "/References/MSHS ED Mapping.xlsx"))

# Data Pre-processing -----------------------------------------------------
# dates originally come in as POSIXct, so they're being converted to Date
dist_dates <- dict_pay_cycles %>%
  select(PP_END_DATE, PREMIER_DISTRIBUTION) %>%
  distinct() %>%
  drop_na() %>%
  arrange(PP_END_DATE) %>%
  #filter only on distribution end dates
  filter(PREMIER_DISTRIBUTION %in% c(TRUE, 1),
         PP_END_DATE < as.POSIXct(Sys.Date() - 7))

#Selecting current and previous distribution dates
distribution <- dist_dates$PP_END_DATE[nrow(dist_dates)]
previous_distribution <- dist_dates$PP_END_DATE[nrow(dist_dates)-1]
#Confirming distribution dates
cat("Current distribution is", format(distribution,"%m/%d/%Y"),
    "\nPrevious distribution is", format(previous_distribution,"%m/%d/%Y"))
answer <- select.list(choices = c("Yes", "No"),
                      preselect = "Yes",
                      multiple = F,
                      title = "Correct distribution?",
                      graphics = T)
if (answer == "No") {
  distribution <- select.list(choices =
                                format(sort.POSIXlt(dist_dates$PP_END_DATE, decreasing = T),
                                       "%m/%d/%Y"),
                              multiple = F,
                              title = "Select current distribution",
                              graphics = T)
  which(distribution == format(dist_dates$PP_END_DATE, "%m/%d/%Y"))
  previous_distribution <- format(dist_dates$PP_END_DATE[which(distribution == 
                                                                 format(dist_dates$PP_END_DATE, "%m/%d/%Y"))-1],"%m/%d/%Y")
}

previous_distribution <- as.Date(previous_distribution, format = "%m/%d/%Y")
distribution <- as.Date(distribution, format = "%m/%d/%Y")

dict_pay_cycles <- dict_pay_cycles %>%
  rename(DATE = PAYCYCLE_DATE,
         START.DATE = PP_START_DATE,
         END.DATE = PP_END_DATE,
         PREMIER.DISTRIBUTION = PREMIER_DISTRIBUTION) %>%
  mutate(DATE = format(as.Date(DATE, format = "%m/%d/%Y"), "%-m/%-d/%Y"),
         START.DATE = format(as.Date(START.DATE, format = "%m/%d/%Y"), "%-m/%-d/%Y"),
         END.DATE = format(as.Date(END.DATE, format = "%m/%d/%Y"), "%-m/%-d/%Y")) %>%
  select(-PREMIER.DISTRIBUTION)

ED_boarder_data <- ED_boarder_data_raw %>%
  left_join(dict_pay_cycles, by = c("Arrival.Date" = "DATE")) %>%
  left_join(mapping, by = c("Arrv.Dept..group." = "Site")) %>%
  filter(Admits == 1) %>%
  mutate(Boarder_Hours = Admit.to.Depart..All. / 60) %>%
  mutate(Required_Care_Hours = Boarder_Hours / 6) %>%
  mutate(Additional_Visits = Required_Care_Hours / WHPU)

ED_boarder_counts <- ED_boarder_data %>%
  group_by(Arrv.Dept..group., END.DATE) %>%
  summarize(total_additional_visits = sum(Additional_Visits, na.rm = TRUE)) %>%
  mutate(END.DATE = as.Date(END.DATE, format = "%m/%d/%Y")) %>%
  filter(END.DATE > previous_distribution & END.DATE <= distribution)

ED_visit_data <- ED_visit_data_raw %>%
  left_join(dict_pay_cycles, by = c("Arrival.Date" = "DATE")) %>%
  left_join(mapping, by = c("Arrv.Dept..group." = "Site"))

ED_visit_counts <- ED_visit_data %>%
  group_by(Arrv.Dept..group., END.DATE) %>%
  summarize(arrival_count = n()) %>%
  mutate(END.DATE = as.Date(END.DATE, format = "%m/%d/%Y")) %>%
  filter(END.DATE > previous_distribution & END.DATE <= distribution)

combined_df <- ED_boarder_counts %>%
  full_join(ED_visit_counts, by = c("Arrv.Dept..group.", "END.DATE")) %>%
  mutate(total_count = total_additional_visits + arrival_count) %>%
  select(Arrv.Dept..group., END.DATE, total_additional_visits, arrival_count, total_count)


# Data Formatting ---------------------------------------------------------
upload <- combined_df %>%
  left_join(mapping, by = c("Arrv.Dept..group." = "Site")) %>%
  mutate(`Corporation Code` = "729805",
         START.DATE = as.Date(END.DATE) - 13,
         `Budget Volume` = "0") %>%
  ungroup() %>%
  mutate(START.DATE = format(as.Date(START.DATE, format = "%m/%d/%Y"), "%-m/%-d/%Y"),
         END.DATE = format(as.Date(END.DATE, format = "%m/%d/%Y"), "%-m/%-d/%Y")) %>%
  select(`Corporation Code`, `Entity Code`, `Cost Center`, START.DATE, END.DATE,
         `Volume ID`, total_count, `Budget Volume`)

upload_cols <- c("Corporation Code",
                 "Entity Code",
                 "Cost Center Code",
                 "Start Date",
                 "End Date",
                 "Volume Code",
                 "Actual Volume",
                 "Budget Volume")

colnames(upload) <- upload_cols
# Quality Checks ----------------------------------------------------------
# Checks that are performed on the output to confirm data consistency and
# expected outputs.
new_data <- combined_df

MSHS_ED_trend_data <- read_excel(paste0(dir, "/Trend/MSHS ED Trend.xlsx")) %>%
  rbind(new_data) %>%
  arrange(END.DATE, Arrv.Dept..group.) %>%
  distinct(END.DATE, Arrv.Dept..group., .keep_all = TRUE)

#Save new data to trend file
write.xlsx(MSHS_ED_trend_data, file = paste0(dir, "/Trend/MSHS ED Trend.xlsx"))

# Plotting
quality_check <- ggplot(MSHS_ED_trend_data, aes(x = END.DATE, y = total_count, color = Arrv.Dept..group.)) +
  geom_line() +
  labs(title = "Quality Check Graph",
       x = "Date",
       y = "Total Count",
       color = "Arrv.Dept..group.") +
  theme_minimal()

# Print the graph
print(quality_check)

# File Saving -------------------------------------------------------------
# Writing files or data for storage
write.csv(upload, file = paste0(dir, "/Uploads/MSHS_ED Nursing_Volumes_", 
                                format(as.Date(previous_distribution) + 1, "%Y-%m-%d"), "_", 
                                format(as.Date(distribution), "%Y-%m-%d"), ".csv"), 
          row.names = FALSE)

# Script End --------------------------------------------------------------