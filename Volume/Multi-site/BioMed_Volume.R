# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)

#push to github

# Initial Set-up of Dataframe ---------------------------------------------
if ("Presidents" %in% list.files("J://")) {
  user_directory <- "J:/Presidents/"
} else {
  user_directory <- "J:/deans/Presidents/"
}

#choose the biomed volume data file
biomed_data_path <- file.choose()

raw_biomed_data <- read_excel(biomed_data_path) %>%
  mutate(`Completion Date/Time` = as.Date(`Completion Date/Time`))

pay_cycle_data <- read_excel(paste0(user_directory,
                                    "SixSigma/MSHS Productivity/Productivity/Universal Data/Mapping/",
                                    "MSHS_Pay_Cycle.xlsx"))

#left join biomed WO data with pay cycle to get start/end dates
clean_data <- select(raw_biomed_data, "Building", "Completion Date/Time") %>%
  left_join(select(pay_cycle_data, c(-PREMIER.DISTRIBUTION)),
            by = c("Completion Date/Time" = "DATE")) %>%
  set_names(c("Building" = "site",
             "Completion Date/Time" = "service_date", 
             "START.DATE" = "pc_start_date",
             "END.DATE" = "pc_end_date"))

#convert <dttm> columns to <date>
date_cols <- c("service_date", "pc_start_date", "pc_end_date")
clean_data[date_cols] <- lapply(clean_data[date_cols], as.Date)

# calculating work order counts per site and date -------------------------

#generate lists of distinct sites, pay cycle start dates and pay cycle end dates
sites <- distinct(clean_data, site)
pc_start_dates <- distinct(clean_data, pc_start_date)
pc_end_dates <- distinct(clean_data, pc_end_date)

#convert lists to vectors
sites_vec <- sites$site
pc_start_dates_vec <- pc_start_dates$pc_start_date
pc_end_dates_vec <- pc_end_dates$pc_end_date

#create "all" vectors, which contain data necessary to produce all rows for upload
if (length(pc_start_dates_vec) == 2) {
  all_sites  <- sort(c(sites_vec, sites_vec))
} else {
  all_sites <- sort(c(sites_vec, sites_vec, sites_vec))
}

all_start_dates <- c(pc_start_dates_vec, pc_start_dates_vec,
                     pc_start_dates_vec, pc_start_dates_vec)

all_end_dates <- c(pc_end_dates_vec, pc_end_dates_vec,
                   pc_end_dates_vec, pc_end_dates_vec)

#filter clean_data by each site
msbi <- filter(clean_data, site == "Mount Sinai Beth Israel")
msh <- filter(clean_data, site == "Mount Sinai Medical Center")
msm <- filter(clean_data, site == "Mount Sinai Morningside")
msw <- filter(clean_data, site == "Mount Sinai West")

#generate work order counts of each site-date combination
msbi_work_orders <- summary(factor(msbi$pc_end_date))
msh_work_orders <- summary(factor(msh$pc_end_date))
msm_work_orders <- summary(factor(msm$pc_end_date))
msw_work_orders <- summary(factor(msw$pc_end_date))

all_work_orders <- c(msbi_work_orders, 
                     msh_work_orders,
                     msm_work_orders,
                     msw_work_orders)

#combine all_sites, all_start_dates, all_end_dates, and all_work_orders into df
summary_df <- data.frame(all_sites,
                         all_start_dates,
                         all_end_dates,
                         all_work_orders) %>%
  set_names(c("Site Name", "Start Date", "End Date", "Volume"))

#create df to store system, site, cost center, and vol IDs, as well as budget and sites_vec
system_id_vec <- c("729805", "729805", "729805", "729805")
site_id_vec <- c("630571", "NY0014", "NY2163", "NY2162")
cost_center_id_vec <- c("401000095460150", "101000010160150", "302000030260150", "301000030160150")
vol_id_vec <- c("401954601501", "101101601501", "302000601501", "301000601501")
budget_vec <- c(0, 0, 0, 0)

ref_df <- data.frame(system_id_vec,
                     site_id_vec,
                     cost_center_id_vec,
                     vol_id_vec,
                     budget_vec,
                     sites_vec) %>%
  set_names(c("System", "Site", "Cost Ctr", "Vol ID", "Budget Vol", "Site Name"))

#combining summary_df and ref_df to create upload df

upload_df <- left_join(summary_df, ref_df,
                       by = c("Site Name")
                       )
#re-ordering columns to match upload format
upload_df <- upload_df[, c(5, 6, 7, 2, 3, 8, 4, 9)]

#export upload_df as a .csv file
earliest_date <- min(pc_start_dates_vec) %>%  
  format("%d%b%y") %>%
  as.character() %>%
  toupper()

latest_date <- max(pc_end_dates_vec) %>%
  format("%d%b%y") %>%
  as.character() %>%
  toupper()
  
write.table(upload_df, 
            file = paste0(user_directory,
                          "SixSigma/MSHS Productivity/Productivity/Volume - Data/Multisite Volumes/Biomed/Uploads/",
                          "multisite_biomed_volumes_",
                          earliest_date, "_to_", latest_date, ".csv"),
            row.names = FALSE,
            col.names = FALSE,
            sep = ","
)

## Creating Trend Graph ---------------------------------------------------
new_data <- summary_df[-2]

#updating column names to match trend data file
colnames(new_data) <- c("site", "end_date", "work_order_count")

#add new data to existing biomed trend data
biomed_trend_data <- read_excel(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/", 
                                       "Volume - Data/Multisite Volumes/Biomed/", 
                                       "biomed_trend_data.xlsx")) %>%
  mutate(end_date = as.Date(end_date)) %>%
  rbind(new_data) %>%
  arrange(end_date, site) %>%
  distinct(site, end_date, .keep_all = TRUE)

#save new data to .xlsx file
write_xlsx(biomed_trend_data, paste0(user_directory,
                                     "SixSigma/MSHS Productivity/Productivity/Volume - Data/Multisite Volumes/Biomed/", 
                                     "biomed_trend_data.xlsx"))

#create plot
ggplot(data = biomed_trend_data,
       mapping = aes(x = end_date, y = work_order_count, color = site)) + 
  geom_line()

## Checking Agency Hours ------------------------------------------------
agency_data <- read_excel(biomed_data_path,
                          sheet = "GE Staff") %>%
  left_join(select(pay_cycle_data, c(-PREMIER.DISTRIBUTION, -START.DATE)),
            by = c("Date" = "DATE")) %>%
  group_by(END.DATE) %>%
  summarise(total_hours = sum(`Worked Hours`)) %>%
  mutate(exceeds_75_hours = ifelse(total_hours >= 75, 1, 0))

#check agency hours and print a message for the user
agency_hours_vec <- as.vector(agency_data$exceeds_75_hours)

if (1 %in% agency_hours_vec){
  print(paste0("One or more pay periods exceed 75 hours of agency work.", 
               "Please review the agency_data df and notify Matt Miesle!"))
  }else{
    print("No pay periods exceed 75 hours of agency work.")
  }