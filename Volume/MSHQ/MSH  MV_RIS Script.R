library(tidyverse)
library(lubridate)
library(readxl)

#read in charge detail 
RIS <- read.csv(file.choose()
                  , colClasses = c(rep("character",21)))

pp_mapping <- read_xlsx(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                               "Productivity/Universal Data/Mapping/",
                               "MSHS_Pay_Cycle.xlsx"))

#Trim whitespace in charge detail

RIS[,1:21] <- sapply(RIS[,1:21], trimws)

mobile_van <- RIS %>%
  
  #pivot longer to combine charge columns into one 
  pivot_longer(cols=c(10:17),
               names_to = "charge",
               values_to = "code") %>%
  
  #remove blanks
  filter(code != "") %>%
  
  #unite columns for final CPT4 code for upload
  unite(CPT4,code, Charge.Mod.1, Charge.Mod.2, Charge.Mod.3, Charge.Mod.4, sep ="") %>%
  
  # create grouping columns  - marc look at this and summarise 
  group_by(Date,CPT4) %>%
  
  #summarize and remove un-necessary columns 
  summarise(volume = n()) %>% 

mutate(Facility = "729805",
       Site = "NY0014",
       DeptID = "MSHRIS21009",
       Date = paste0(substr(Date,6,7), "/",
                     substr(Date,9,10), "/",
                     substr(Date,1,4)),
       Budget = "0") %>%
  #Create second date column
  mutate(Date2 = Date) %>%
  #select column order for upload
  select(Facility, Site, DeptID, Date, Date2, CPT4, volume, Budget)


#saveRDS(mobile_van, file = "Master.rds")


#----------------MASTER-----------------------------------------------
old_master <- readRDS(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                             "Productivity/Volume - Data/MSH Data/RIS/Master/",
                             "Mobile Van/Master.rds"))

#Check that the new upload does not overlap with master
if(max(as.Date(old_master$Date2,format = "%m/%d/%Y")) < min(as.Date(mobile_van$Date,format = "%m/%d/%Y"))){
  new_master <- rbind(old_master, mobile_van)
} else {
  stop("Raw data overlaps with master")
}

#Quality check----------------------------------------------------------
#Read in old trend
old_trend_master <- readRDS(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                                   "Productivity/Volume - Data/MSH Data/RIS/Master/",
                                   "Mobile Van/Trend_Master.rds"))

#Format date columns in same format as new_master
pp_mapping$DATE <- format(as.Date(pp_mapping$DATE), "%m/%d/%Y")
pp_mapping$END.DATE <- format(as.Date(pp_mapping$END.DATE), "%m/%d/%Y")


pp_mapping[, 1] <- sapply(pp_mapping[, 1], as.character) #Convert to character

#Creates volume trend
trend <- new_master %>%
  left_join(pp_mapping, by = c('Date2' = 'DATE')) %>% #Adds PP end date column
  ungroup() %>%
  group_by(DeptID,END.DATE) %>% 
  summarise(Vol = sum(volume, na.rm = T)) %>% 
  pivot_wider(id_cols = c(DeptID),names_from = END.DATE, values_from = Vol)

View(trend)

#append current trend to master trend and saves
new_trend_master <- rbind(old_trend_master, trend)
saveRDS(new_trend_master, paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                                 "Productivity/Volume - Data/MSH Data/RIS/Master/",
                                 "Mobile Van/Trend_Master.rds"))

#Save new master 
saveRDS(new_master, paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                           "Productivity/Volume - Data/MSH Data/RIS/Master/",
                           "Mobile Van/Master.rds"))

#save upload

min_date <- min(as.Date(mobile_van$Date, format = "%m/%d/%Y"))
min_mon <- toupper(month.abb[month(min_date)])
min_date_save <- paste0(substr(min_date,9,10),
                        min_mon,
                        substr(min_date,1,4))

max_date <- max(as.Date(mobile_van$Date, format = "%m/%d/%Y"))
max_mon <- toupper(month.abb[month(min_date)])
max_date_save <- paste0(substr(max_date,9,10),
                        min_mon,
                        substr(max_date,1,4))
write.csv(mobile_van, paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                          "Productivity/Volume - Data/MSH Data/RIS/Uploads/",
                          "Mobile Van/MSH_Mobile Van RIS_",
                          min_date_save, " to ",max_date_save, ".csv"),
          sep=",",row.names = F, col.names = F) #--- ask Greg about col names issue 



