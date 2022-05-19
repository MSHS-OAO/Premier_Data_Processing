library(tidyverse)
library(readxl)
library(lubridate)

#read in charge detail and cdm
RIS <- read.csv(file.choose(), 
                              colClasses = c(rep("character",21)))
CDM <- read.csv(file.choose(), 
                    colClasses = c(rep("character",3))) 

#Trim whitespace in charge detail
RIS[,1:21] <- sapply(RIS[,1:21], trimws)

nuc_med <- RIS %>%
  #filter out any rows with PP in Location Column and 86 Visit column 
  filter(location != "PP",
         Visit != "86") %>%
  #pivot longer to combine charge columns into one
  pivot_longer(cols = c(10:17), 
               names_to = "charge", 
               values_to = "code") %>%
  #remove blanks
  filter(code != "") %>%
  #add identifier
  mutate(complete_identifier = "154") %>%
  #combine identifier and code column
  unite(charge_code, complete_identifier, code, sep = "") %>%
  #join with CDM to get cpt4 code associated with charge code
  left_join(CDM, by = c("charge_code" = "CHARGE_CODE_OUT")) %>%
  filter(!is.na(OPTB_cpt4)) %>%
  #unite columns for final CPT4 code for upload
  unite(CPT4, 
        OPTB_cpt4, Charge.Mod.1, Charge.Mod.2, Charge.Mod.3, Charge.Mod.4, 
        sep = "") %>%
  #create grouping columns
  group_by(Date, CPT4) %>%
  #summarize and remove unnecessary columsn
  summarise(volume = n()) %>%
  #Add in necessary columns for upload
  mutate(Facility = "729805",
         Site = "NY0014",
         DeptID = "MSHRIS21011",
         Date = paste0(substr(Date,6,7), "/",
                       substr(Date,9,10), "/",
                       substr(Date,1,4)),
         Budget = "0") %>%
  #Create second date column
  mutate(Date2 = Date) %>%
  #select column order for upload
  select(Facility, Site, DeptID, Date, Date2, CPT4, volume, Budget)

min_date <- min(as.Date(nuc_med$Date, format = "%m/%d/%Y"))
min_mon <- toupper(month.abb[month(min_date)])
min_date_save <- paste0(substr(min_date,9,10),
                        min_mon,
                        substr(min_date,1,4))
max_date <- max(as.Date(nuc_med$Date, format = "%m/%d/%Y"))
max_mon <- toupper(month.abb[month(min_date)])
max_date_save <- paste0(substr(max_date,9,10),
                        min_mon,
                        substr(max_date,1,4))
write.csv(nuc_med, paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                          "Productivity/Volume - Data/MSH Data/RIS/Uploads/",
                          "Nuc Med/MSH_Nuc Med RIS_",
                          min_date_save, " to ",max_date_save, ".csv"))
#---------MASTER (Daniel hold off un running until like 65!!!!!)---------------
#read old master
old_master <- readRDS(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                             "Productivity/Volume - Data/MSH Data/RIS/Master/",
                             "Nuc Med/Master.rds"))


#append current upload to master
new_master <- rbind(old_master, nuc_med)
#save new master
saveRDS(new_master, paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                        "Productivity/Volume - Data/MSH Data/RIS/Master/",
                        "Nuc Med/Master.rds"))

#Quality Check
pp_mapping <- read_xlsx(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                               "Productivity/Universal Data/Mapping/",
                               "MSHS_Pay_Cycle.xlsx"))

pp_mapping$DATE <- format(as.Date(pp_mapping$DATE), "%m/%d/%Y")
pp_mapping$END.DATE <- format(as.Date(pp_mapping$END.DATE), "%m/%d/%Y")

pp_mapping[, 1] <- sapply(pp_mapping[, 1], as.character)

trend <- new_master %>%
  left_join(pp_mapping, by = c('Date2' = 'DATE')) %>% 
  ungroup() %>%
  group_by(DeptID,END.DATE) %>%
  summarise(Vol = sum(volume, na.rm = T)) %>%
  pivot_wider(id_cols = c(DeptID),names_from = END.DATE, values_from = Vol)

View(trend)

#save upload
min_date <- min(as.Date(nuc_med$Date, format = "%m/%d/%Y"))
min_mon <- toupper(month.abb[month(min_date)])
min_date_save <- paste0(substr(min_date,9,10),
                        min_mon,
                        substr(min_date,1,4))
max_date <- max(as.Date(nuc_med$Date, format = "%m/%d/%Y"))
max_mon <- toupper(month.abb[month(min_date)])
max_date_save <- paste0(substr(max_date,9,10),
                        min_mon,
                        substr(max_date,1,4))
write.csv(nuc_med, paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                          "Productivity/Volume - Data/MSH Data/RIS/Uploads/",
                          "Nuc Med/MSH_Nuc Med RIS_",
                          min_date_save, " to ",max_date_save, ".csv"))
  


