library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)

#MSH RIS directory
RIS_dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                  "Productivity/Volume - Data/MSH Data/RIS/")

#MSQ RIS directory
MSQ_RIS_dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                  "Productivity/Volume - Data/MSQ Data/RIS/")

#month and year of charge detail
month_year <- "MAY2022"
#Diagnostic OR scaling factor
diagnostic_scaling <- 2.69

#read in MSH RIS charge detail
RIS <- read.csv(paste0(RIS_dir, "Charge Detail/MSH_RIS_", month_year, ".csv"), 
                colClasses = c(rep("character", 21)))
#read in OR Diagnostic
RIS_OR <- read.csv(paste0(RIS_dir,"Charge Detail/OR Diagnostic/",
                          "MSH_RIS_OR Diagnostic_",month_year,".csv"), 
                   colClasses = c(rep("character",9))) %>%
  select(MRN, Name, ACC, Date, Exam, Exam.Modifier, Org, Resource)
#read in neuro detail
RIS_neuro <- read.csv(paste0(RIS_dir,"Charge Detail/Neurosurgery/",
                             "MSH_Neuro_",month_year,".csv"), 
                      colClasses = c(rep("character",21)))
#read in Nuc Med
RIS_nuc_med <- read.csv(paste0(RIS_dir, "Charge Detail/Nuc Med/", month_year, 
                               " NM.csv"), 
                colClasses = c(rep("character", 21)))
#read in Nud Med CDM
MSH_CDM <- read.csv(paste0(RIS_dir, "CDM/Nuc Med CDM.csv"),
                        colClasses = c(rep("character", 3)))
#read in MV file
RIS_MV <- read.csv(paste0(RIS_dir, "Charge Detail/MV/", month_year, " MV.csv"),
                   colClasses = c(rep("character", 21)))
#read in MSQ RIS Charge detail
RIS_MSQ <- read.csv(paste0(MSQ_RIS_dir,"Charge Detail/",
                       "MSQ_RIS_",month_year,".csv"), 
                colClasses = c(rep("character",21)))
#read in MSQ CDM
MSQ_CDM <- read_xlsx(paste0(MSQ_RIS_dir, "CDM/MSQ CDM.xlsx"), 
                     col_types = rep("text",55))
#Read in modality mapping file
modality <- read_xlsx(paste0(RIS_dir,"Mapping/Modality_Mapping.xlsx"))
#Read in PAT mapping
PAT <- read_xlsx(paste0(RIS_dir,"Mapping/PAT_Mapping.xlsx"))
#Premier Dep ID for CPT upload
Premier_Dep <- read_xlsx(paste0(RIS_dir,"Mapping/Premier_ID.xlsx"))

#Read in pay period mapping for trend
pp_mapping <- read_xlsx(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                               "Productivity/Universal Data/Mapping/",
                               "MSHS_Pay_Cycle.xlsx"))

#Read in department mapping for trend
dept_mapping <- read_xlsx(paste0(RIS_dir,"/Mapping/",
                                 "Dept_Description_Mapping.xlsx"))

#Read in CPT reference for trend mapping and selects columns needed
cpt_mapping <- read_xlsx(paste0(RIS_dir,"/Mapping/",
                                "CPT_Ref.xlsx")) %>%
  select(1, 2, 3, 6, 12)

#read in Dep ID mapping for MSQ
Dep_ID <- read_xlsx(paste0(MSQ_RIS_dir,"Mapping/Dep_ID.xlsx"),
                    col_types = c("text","text"))
#remove whitespaces
RIS[,1:21] <- sapply(RIS[,1:21], trimws)
RIS_neuro[,1:21] <- sapply(RIS_neuro[,1:21], trimws)
RIS_nuc_med[,1:21] <- sapply(RIS_nuc_med[,1:21], trimws)
RIS_MV[,1:21] <- sapply(RIS_MV[,1:21], trimws)
RIS_MSQ[,1:21] <- sapply(RIS_MSQ[,1:21], trimws)

#---------------------------------------MSH RIS-----------------------------
#modifiers we want to keep
acceptable_modifiers <- c("26","50","53","tc")

#pivot charge columns longer into a single column
RIS_charge <- RIS %>% 
  pivot_longer(cols = c(10:17), names_to = "Charge", values_to = "charge_code") %>%
  filter(charge_code != "") %>%
  select(-Charge)%>%
  mutate(
    Charge.Mod.1 = case_when(
      Charge.Mod.1 %in% acceptable_modifiers ~ Charge.Mod.1,
      TRUE ~ ""),
    Charge.Mod.2 = case_when(
      Charge.Mod.2 %in% acceptable_modifiers ~ Charge.Mod.2,
      TRUE ~ ""),
    Charge.Mod.3 = case_when(
      Charge.Mod.3 %in% acceptable_modifiers ~ Charge.Mod.3,
      TRUE ~ ""),
    Charge.Mod.4 = case_when(
      Charge.Mod.4 %in% acceptable_modifiers ~ Charge.Mod.4,
      TRUE ~ ""),
    Modifier = paste0(Charge.Mod.1,Charge.Mod.2,Charge.Mod.3,Charge.Mod.4)) %>%
  unite(CPT4, 
        charge_code, Charge.Mod.1, Charge.Mod.2, Charge.Mod.3, Charge.Mod.4, 
        sep = "") %>%
  left_join(modality) %>%
  left_join(PAT, by = c("Pat.Type" = "Code")) %>%
  mutate(Identifier = paste0(Org,"-",Modality,"-",`IP or OP`)) %>%
  left_join(Premier_Dep) %>%
  mutate(Partner = "729805",
         Hosp = "NY0014",
         Start = paste0(substr(Date,6,7), "/",
                        substr(Date,9,10), "/",
                        substr(Date,1,4)),
         End = paste0(substr(Date,6,7), "/",
                      substr(Date,9,10), "/",
                      substr(Date,1,4))) %>%
  select(Partner, Hosp, DepID, Start, End, CPT4)

#-------------------------------OR Diagnostic---------------------------------
#prepare OR diagnostic volume
RIS_OR_upload <- RIS_OR %>%
  filter(Org == "RM") %>%
  mutate(Partner = "729805",
         Hosp = "NY0014",
         DepID = "MSHRIS21008OR",
         Start = mdy(Date),
         End = mdy(Date),
         CPT4 = "71045") %>%
  select(Partner, Hosp, DepID, Start, End, CPT4) 
RIS_OR_upload <- RIS_OR_upload %>%
  mutate(Start = paste0(substr(Start,6,7), "/",
                        substr(Start,9,10), "/",
                        substr(Start,1,4)),
         End = paste0(substr(End,6,7), "/",
                      substr(End,9,10), "/",
                      substr(End,1,4)))

#-------------------------------Neuro-----------------------------------------
#prepare neuro volume
neuro <- RIS_neuro %>%
  #pivot longer to combine charge columns into one
  pivot_longer(cols = c(10:17), 
               names_to = "charge", 
               values_to = "code") %>%
  #remove blank charges from upload prep
  filter(code != "") %>%
  #drop charge column
  select(-charge) %>%
  #reome unwated modifier
  mutate(
    Charge.Mod.1 = case_when(
      Charge.Mod.1 %in% acceptable_modifiers ~ Charge.Mod.1,
      TRUE ~ ""),
    Charge.Mod.2 = case_when(
      Charge.Mod.2 %in% acceptable_modifiers ~ Charge.Mod.2,
      TRUE ~ ""),
    Charge.Mod.3 = case_when(
      Charge.Mod.3 %in% acceptable_modifiers ~ Charge.Mod.3,
      TRUE ~ ""),
    Charge.Mod.4 = case_when(
      Charge.Mod.4 %in% acceptable_modifiers ~ Charge.Mod.4,
      TRUE ~ "")) %>%
  #unite charge and 
  unite(CPT4, 
        code, Charge.Mod.1, Charge.Mod.2, Charge.Mod.3, Charge.Mod.4, 
        sep = "") %>%
  #Add in necessary columns for upload
  mutate(Partner = "729805",
         Hosp = "NY0014",
         DepID = "MSHRIS21050",
         Start = paste0(
           substr(Date, 6, 7), "/",
           substr(Date, 9, 10), "/",
           substr(Date, 1, 4))) %>%
  mutate(End = Start) %>%
  #select column order for upload
  select(Partner, Hosp, DepID, Start, End, CPT4)

#bind both files for upload
MSH_upload <- rbind(RIS_charge, neuro, RIS_OR_upload) %>%
  group_by(Partner, Hosp, DepID, Start, End, CPT4) %>%
  summarise(volume = n()) %>%
  mutate(budget = "0",
         volume = as.numeric(volume))
MSH_upload <- MSH_upload %>%
  mutate(volume = case_when(
    DepID == "MSHRIS21008OR" ~ volume * diagnostic_scaling,
    TRUE ~ volume))

#-----------------------------------Nuc Med----------------------------------
nuc_med <- RIS_nuc_med %>%
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
  left_join(MSH_CDM, by = c("charge_code" = "CHARGE_CODE_OUT")) %>%
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

#---------------------------Mobile Van-----------------------------------------
mobile_van <- RIS_MV %>%
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

#----------------------------MSQ RIS-------------------------------------------
#pivot charge columns longer into a single column
RIS_MSQ_charge <- RIS_MSQ %>% 
  pivot_longer(cols = c(10:17), names_to = "Charge", values_to = "charge_code") %>%
  filter(charge_code != "") %>%
  select(-Charge)
#modifiers we want to keep
acceptable_modifiers <- c("26","50","53","tc")
#remove unwanted modifiers and concatenate remaining
RIS_MSQ_charge_mod <- RIS_MSQ_charge %>%
  mutate(
    Charge.Mod.1 = case_when(
      Charge.Mod.1 %in% acceptable_modifiers ~ Charge.Mod.1,
      TRUE ~ ""),
    Charge.Mod.2 = case_when(
      Charge.Mod.2 %in% acceptable_modifiers ~ Charge.Mod.2,
      TRUE ~ ""),
    Charge.Mod.3 = case_when(
      Charge.Mod.3 %in% acceptable_modifiers ~ Charge.Mod.3,
      TRUE ~ ""),
    Charge.Mod.4 = case_when(
      Charge.Mod.4 %in% acceptable_modifiers ~ Charge.Mod.4,
      TRUE ~ ""),
    Modifier = paste0(Charge.Mod.1,Charge.Mod.2,Charge.Mod.3,Charge.Mod.4))

#select columns from CDM for join
MSQ_CDM_join <- MSQ_CDM %>%
  select(`CHARGE CODE`,`CHARGE DESC`,`DEPARTMENT CODE`,`GENERAL CPT4 CODE`) %>%
  rename(charge_code = `CHARGE CODE`) %>%
  distinct()
#join charge detail with CDM, PAT, and DEP IT
RIS_MSQ_cpt4 <- left_join(RIS_MSQ_charge_mod, MSQ_CDM_join) %>%
  filter(!is.na(`GENERAL CPT4 CODE`)) %>%
  left_join(PAT, by = c("Pat.Type" = "Code")) %>%
  left_join(Dep_ID, by = c("DEPARTMENT CODE" = "Dept")) %>%
  mutate(Identifier = paste0(Org,"-",`DEPARTMENT CODE`,"-",`IP or OP`)) %>%
  mutate(CPT = paste0(`GENERAL CPT4 CODE`,Modifier)) %>%
  left_join(Premier_Dep) %>%
  mutate(Start = paste0(
    substr(Date,6,7),"/",
    substr(Date,9,10),"/",
    substr(Date,1,4))) %>%
  mutate(End = Start,
         Partner = "729805",
         Hosp = "NY0014") %>%
  mutate(Start = mdy(Start),
         End = mdy(End))

#prepare OR diagnostic volume
RIS_MSQ_OR_upload <- RIS_OR %>%
  filter(Org == "QN") %>%
  mutate(Partner = "729805",
         Hosp = "NY0014",
         DepID = "MSQRIS21008OR",
         Start = mdy(Date),
         End = mdy(Date),
         CPT = "71045") %>%
  group_by(Partner,Hosp,DepID,Start,End,CPT) %>%
  summarise(Volume = n()) %>%
  mutate(Budget = "0")

#prepare upload
RIS_MSQ_cpt4_upload <- RIS_MSQ_cpt4 %>%
  group_by(Partner,Hosp,DepID,Start,End,CPT) %>%
  summarise(Volume = n()) %>%
  mutate(Budget = "0")

#combine upload files
MSQ_upload <- rbind(RIS_MSQ_cpt4_upload, RIS_MSQ_OR_upload) %>%
  mutate(
    Start = paste0(
      substr(Start,6,7),"/",
      substr(Start,9,10),"/",
      substr(Start,1,4)),
    End = paste0(
      substr(End,6,7),"/",
      substr(End,9,10),"/",
      substr(End,1,4)))

#---------------------------MSH_RIS Master and Trend---------------------------
old_MSH_master <- readRDS(paste0(RIS_dir,"Master/Master.rds"))
if(max(mdy(old_MSH_master$End)) < min(mdy(MSH_upload$Start))){
  new_master <- rbind(as.data.frame(old_MSH_master),
                      as.data.frame(MSH_upload))
} else {
  stop("Raw data overlaps with master")
}
saveRDS(new_MSH_master,paste0(RIS_dir,"Master/Master.rds"))
####################################################

#Trend Check
#Getting quarters from dates
new_MSH_master$qrts <- quarters(mdy(new_MSH_master$End))

#Creates a trend of the master biweekly to verify data is accurate
MSH_trend <- new_MSH_master %>%
  mutate(End = mdy(End)) %>%
  mutate(`Concatenate for lookup` = paste0(substr(End,1,4), qrts, CPT4), 
         volume = as.numeric(volume)) %>%
  left_join(.,cpt_mapping) %>%
  filter(!is.na(`Facility Practice Expense RVU Factor`) | !is.na(`CPT Procedure Count`)) %>%
  left_join(pp_mapping, by = c("End" = 'DATE')) %>%
  left_join(dept_mapping, by = c("DepID" = "Department.ID")) %>%
  mutate(True_Volume = case_when(
    CPT.Group == "Procedure" ~ volume * `CPT Procedure Count`,
    CPT.Group == "RVU" ~ volume * `Facility Practice Expense RVU Factor`)) %>%
  ungroup() %>%
  group_by(DepID, Department.Description, CPT.Group, END.DATE) %>%
  #na.omit() %>% #Removes NA department
  summarise(Vol = sum(True_Volume, na.rm = T)) %>%
  arrange(END.DATE) %>%
  pivot_wider(id_cols = c(DepID, Department.Description, CPT.Group),names_from = END.DATE, values_from = Vol)

View(MSH_trend)

#Save master trend
saveRDS(MSH_trend, paste0(RIS_dir,"Master/Master_Trend.rds"))

#--------------------------Nuc Med Master and Trend----------------------------
#read old master
old_nm_master <- readRDS(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                             "Productivity/Volume - Data/MSH Data/RIS/Master/",
                             "Nuc Med/Master.rds"))

#append current upload to master
new_nm_master <- rbind(old_nm_master, nuc_med)
#save new master
saveRDS(new_nm_master, paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                           "Productivity/Volume - Data/MSH Data/RIS/Master/",
                           "Nuc Med/Master.rds"))

#Quality Check
pp_mapping <- read_xlsx(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                               "Productivity/Universal Data/Mapping/",
                               "MSHS_Pay_Cycle.xlsx"))

pp_mapping$DATE <- format(as.Date(pp_mapping$DATE), "%m/%d/%Y")
pp_mapping$END.DATE <- format(as.Date(pp_mapping$END.DATE), "%m/%d/%Y")

pp_mapping[, 1] <- sapply(pp_mapping[, 1], as.character)

nm_trend <- new_nm_master %>%
  left_join(pp_mapping, by = c('Date2' = 'DATE')) %>% 
  ungroup() %>%
  group_by(DeptID,END.DATE) %>%
  summarise(Vol = sum(volume, na.rm = T)) %>%
  pivot_wider(id_cols = c(DeptID),names_from = END.DATE, values_from = Vol)

View(nm_trend)

#---------------------------Mobile Van Master and Trend------------------------
old_mv_master <- readRDS(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                             "Productivity/Volume - Data/MSH Data/RIS/Master/",
                             "Mobile Van/Master.rds"))

#Check that the new upload does not overlap with master
if(max(as.Date(old_mv_master$Date2,format = "%m/%d/%Y")) < min(as.Date(mobile_van$Date,format = "%m/%d/%Y"))){
  new_mv_master <- rbind(old_mv_master, mobile_van)
} else {
  stop("Raw data overlaps with master")
}
#Read in old trend
old_mv_trend <- readRDS(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                                   "Productivity/Volume - Data/MSH Data/RIS/Master/",
                                   "Mobile Van/Trend_Master.rds"))

#Format date columns in same format as new_master
pp_mapping$DATE <- format(as.Date(pp_mapping$DATE), "%m/%d/%Y")
pp_mapping$END.DATE <- format(as.Date(pp_mapping$END.DATE), "%m/%d/%Y")


pp_mapping[, 1] <- sapply(pp_mapping[, 1], as.character) #Convert to character

#Creates volume trend
mv_trend <- new_mv_master %>%
  left_join(pp_mapping, by = c('Date2' = 'DATE')) %>% #Adds PP end date column
  ungroup() %>%
  group_by(DeptID,END.DATE) %>% 
  summarise(Vol = sum(volume, na.rm = T)) %>% 
  pivot_wider(id_cols = c(DeptID),names_from = END.DATE, values_from = Vol)

View(mv_trend)

#append current trend to master trend and saves
new_mv_trend <- rbind(old_mv_trend, mv_trend)
saveRDS(new_mv_trend, paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                                 "Productivity/Volume - Data/MSH Data/RIS/Master/",
                                 "Mobile Van/Trend_Master.rds"))

#Save new master 
saveRDS(new_mv_master, paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                           "Productivity/Volume - Data/MSH Data/RIS/Master/",
                           "Mobile Van/Master.rds"))

#------------------------------MSQ RIS Master and Trend-----------------------

##################need to create master append code  
old_MSQ_master <- readRDS(paste0(MSQ_RIS_dir,"Master/New/Master.rds"))
if(max(mdy(old_MSQ_master$End)) < min(mdy(MSQ_upload$Start))){
  new_MSQ_master <- rbind(as.data.frame(old_MSQ_master),
                      as.data.frame(MSQ_upload))
} else {
  stop("Raw data overlaps with master")
}
saveRDS(new_MSQ_master,paste0(MSQ_RIS_dir,"Master/New/Master.rds"))
####################################################

#Trend Check
#Getting quarters from dates
new_MSQ_master$qrts <- quarters(mdy(new_MSQ_master$End))

#Creates a trend of the master biweekly to verify data is accurate
MSQ_trend <- new_MSQ_master %>%
  mutate(End = mdy(End)) %>%
  mutate(`Concatenate for lookup` = paste0(substr(End,1,4), qrts, CPT), 
         Volume = as.numeric(Volume)) %>%
  left_join(.,cpt_mapping) %>%
  filter(!is.na(`Facility Practice Expense RVU Factor`) | !is.na(`CPT Procedure Count`)) %>%
  left_join(pp_mapping, by = c("End" = 'DATE')) %>%
  left_join(dept_mapping, by = c("DepID" = "Department.ID")) %>%
  mutate(True_Volume = case_when(
    CPT.Group == "Procedure" ~ Volume * `CPT Procedure Count`,
    CPT.Group == "RVU" ~ Volume * `Facility Practice Expense RVU Factor`)) %>%
  ungroup() %>%
  group_by(DepID, Department.Description, CPT.Group, END.DATE) %>%
  #na.omit() %>% #Removes NA department
  summarise(Vol = sum(True_Volume, na.rm = T)) %>%
  arrange(END.DATE) %>%
  pivot_wider(id_cols = c(DepID, Department.Description, CPT.Group),names_from = END.DATE, values_from = Vol)

View(MSQ_trend)

#Save master trend
saveRDS(MSQ_trend, paste0(MSQ_RIS_dir,"Master/New/Master_Trend.rds"))

#-------------------------Saving Uploads--------------------------------------
#save MSH upload
write.table(MSH_upload,paste0(RIS_dir,"Uploads/MSH_RIS_",month_year,".csv"),
            sep = ",", row.names = F, col.names = F)

#save Nuc Med upload
min_nm_date <- min(as.Date(nuc_med$Date, format = "%m/%d/%Y"))
min_nm_mon <- toupper(month.abb[month(min_nm_date)])
min_nm_date_save <- paste0(substr(min_nm_date,9,10),
                        min_nm_mon,
                        substr(min_nm_date,1,4))
max_nm_date <- max(as.Date(nuc_med$Date, format = "%m/%d/%Y"))
max_nm_mon <- toupper(month.abb[month(min_nm_date)])
max_nm_date_save <- paste0(substr(max_nm_date,9,10),
                        min_nm_mon,
                        substr(max_nm_date,1,4))
write.csv(nuc_med, paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                          "Productivity/Volume - Data/MSH Data/RIS/Uploads/",
                          "Nuc Med/MSH_Nuc Med RIS_",
                          min_nm_date_save, " to ",max_nm_date_save, ".csv"))

#save mobile van upload
min_mv_date <- min(as.Date(mobile_van$Date, format = "%m/%d/%Y"))
min_mv_mon <- toupper(month.abb[month(min_mv_date)])
min_mv_date_save <- paste0(substr(min_mv_date,9,10),
                        min_mv_mon,
                        substr(min_mv_date,1,4))

max_mv_date <- max(as.Date(mobile_van$Date, format = "%m/%d/%Y"))
max_mv_mon <- toupper(month.abb[month(min_mv_date)])
max_mv_date_save <- paste0(substr(max_mv_date,9,10),
                        min_mv_mon,
                        substr(max_mv_date,1,4))
write.csv(mobile_van, paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                             "Productivity/Volume - Data/MSH Data/RIS/Uploads/",
                             "Mobile Van/MSH_Mobile Van RIS_",
                             min_mv_date_save, " to ",max_mv_date_save, ".csv"),
          sep=",",row.names = F, col.names = F)

#Save MSQ Upload
write.table(MSQ_upload,paste0(MSQ_RIS_dir,"Uploads/new_2022/MSQ_RIS_",month_year,".csv"),
            sep = ",", row.names = F, col.names = F)
