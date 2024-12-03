library(readxl)
library(here)
library(tidyverse)
library(xlsx)
library(lubridate)
library(dplyr)

#MSHQ RIS directory
MSHQ_RIS_dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                       "Productivity/Volume - Data/Multisite Volumes/MSHQ Radiology/")

#month and year of charge detail
month_year <- "MAR2023"

#Diagnostic OR scaling factor
diagnostic_scaling <- 2.69

#--------------Reading in MSH raw data------------------------------------------
#read in charge detail
MSH_RIS <- read.csv(paste0(MSHQ_RIS_dir, "Charge Detail/MSH/MSH_RIS_", 
                           month_year, ".csv"), 
                    colClasses = c(rep("character", 21)))
#read in OR Diagnostic
MSH_RIS_OR <- read.csv(paste0(MSHQ_RIS_dir,"Charge Detail/MSH/OR Diagnostic/",
                          "MSH_RIS_OR Diagnostic_",month_year,".csv"), 
                   colClasses = c(rep("character",9))) %>%
  select(MRN, Name, ACC, Date, Exam, Exam.Modifier, Org, Resource)

#read in neuro detail
MSH_RIS_neuro <- read.csv(paste0(MSHQ_RIS_dir,"Charge Detail/MSH/Neurosurgery/",
                             "MSH_Neuro_",month_year,".csv"), 
                      colClasses = c(rep("character",21)))

#-------------Reading in MSQ raw data------------------------------------------
#read in charge detail
MSQ_RIS <- read.csv(paste0(MSHQ_RIS_dir,"Charge Detail/MSQ/",
                       "MSQ_RIS_",month_year,".csv"), 
                colClasses = c(rep("character",21)))   
#read in OR Diagnostic
MSQ_RIS_OR <- read.csv(paste0(MSQ_RIS_dir,"Charge Detail/MSQ/OR Diagnostic/",
                          "MSQ_RIS_OR Diagnostic_",month_year,".csv"), 
                   colClasses = c(rep("character",9))) %>%
  select(MRN, Name, ACC, Date, Exam, Exam.Modifier, Org, Resource)

#------------Reading in MSH Mapping files--------------------------------------
#Read in modality mapping file
modality <- read_xlsx(paste0(MSHQ_RIS_dir,"Mapping/MSH/Modality_Mapping.xlsx"))

#-------Reading in MSQ Mapping files-------------------------------------------
#read in Resource mapping
Resource <- read_xlsx(paste0(MSHQ_RIS_dir,"Mapping/MSQ/Resource_Mapping.xlsx"),
                      col_types = c("text","text","text"))
#read in CDM
CDM <- read_xlsx(file.choose(), col_types = rep("text",21))

#-------Reading in MSHQ Mapping files------------------------------------------
#Read in CPT reference for trend mapping and selects columns needed
cpt_mapping <- read_xlsx(paste0(MSHQ_RIS_dir,"/Mapping/Both Sites/",
                                "CPT_Ref.xlsx")) %>%
  select(1, 2, 3, 6, 12)

#Read in PAT mapping
PAT <- read_xlsx(paste0(MSHQ_RIS_dir,"Mapping/Both Sites/PAT_Mapping.xlsx"))

#Premier Dep ID for CPT upload
Premier_Dep <- read_xlsx(paste0(MSHQ_RIS_dir,"Mapping/Both Sites/MSHQ_Premier_Dep.xlsx"))

#Read in pay period mapping for trend
pp_mapping <- read_xlsx(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                               "Productivity/Universal Data/Mapping/",
                               "MSHS_Pay_Cycle.xlsx"))

#Read in department mapping for trend
dept_mapping <- read_xlsx(paste0(MSHQ_RIS_dir,"/Mapping/Both Sites",
                                 "MSHQ_Dept_Description_Mapping.xlsx"))

#----------------MSH Processing-----------------------------------------------
#remove whitespaces
MSH_RIS[,1:21] <- sapply(MSH_RIS[,1:21], trimws)
MSH_RIS_neuro[,1:21] <- sapply(MSH_RIS_neuro[,1:21], trimws)

#modifiers we want to keep
acceptable_modifiers <- c("26","50","53","tc")

#pivot charge columns longer into a single column
MSH_RIS_charge <- MSH_RIS %>% 
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

#prepare OR diagnostic volume
MSH_RIS_OR_upload <- MSH_RIS_OR %>%
  filter(Org == "RM") %>%
  mutate(Partner = "729805",
         Hosp = "NY0014",
         DepID = "MSHRIS21008OR",
         Start = mdy(Date),
         End = mdy(Date),
         CPT4 = "71045") %>%
  select(Partner, Hosp, DepID, Start, End, CPT4) 
MSH_RIS_OR_upload <- MSH_RIS_OR_upload %>%
  mutate(Start = paste0(substr(Start,6,7), "/",
                        substr(Start,9,10), "/",
                        substr(Start,1,4)),
         End = paste0(substr(End,6,7), "/",
                      substr(End,9,10), "/",
                      substr(End,1,4)))
#prepare neuro volume
MSH_neuro <- MSH_RIS_neuro %>%
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
MSH_upload <- rbind(MSH_RIS_charge, MSH_neuro, MSH_RIS_OR_upload) %>%
  group_by(Partner, Hosp, DepID, Start, End, CPT4) %>%
  summarise(volume = n()) %>%
  mutate(budget = "0",
         volume = as.numeric(volume))
MSH_upload <- MSH_upload %>%
  mutate(volume = case_when(
    DepID == "MSHRIS21008OR" ~ volume * diagnostic_scaling,
    TRUE ~ volume))

####################################################  
old_MSH_master <- readRDS(paste0(MSHQ_RIS_dir,"Master/MSH/MSH_Master.rds"))
if(max(mdy(old_MSH_master$End)) < min(mdy(MSH_upload$Start))){
  new_MSH_master <- rbind(as.data.frame(old_MSH_master),
                      as.data.frame(MSH_upload))
} else {
  stop("Raw data overlaps with master")
}
saveRDS(new_MSH_master,paste0(MSHQ_RIS_dir,"Master/MSH/MSH_Master.rds"))
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
saveRDS(MSH_trend, paste0(MSHQ_RIS_dir,"Master/MSH/MSH_Master_Trend.rds"))

#save upload
write.table(MSH_upload,paste0(MSHQ_RIS_dir,"Uploads/MSH/MSH_RIS_",month_year,".csv"),
            sep = ",", row.names = F, col.names = F)

#-----------------MSQ Processing------------------------------------------------
#remove whitespaces
MSQ_RIS[,1:21] <- sapply(MSQ_RIS[,1:21], trimws)
#pivot charge columns longer into a single column
MSQ_RIS_charge <- MSQ_RIS %>% 
  pivot_longer(cols = c(10:17), names_to = "Charge", values_to = "charge_code") %>%
  filter(charge_code != "") %>%
  select(-Charge)
#modifiers we want to keep
acceptable_modifiers <- c("26","50","53","tc")
#remove unwanted modifiers and concatenate remaining
MSQ_RIS_charge_mod <- MSQ_RIS_charge %>%
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
CDM_join <- CDM %>%
  select(Code, description, AlternateCode) %>%
  rename(charge_code = Code) %>%
  distinct()
#join charge detail with CDM, PAT, and DEP IT
MSQ_RIS_cpt4 <- left_join(MSQ_RIS_charge_mod, CDM_join) %>%
  filter(!is.na(AlternateCode)) %>%
  left_join(PAT, by = c("Pat.Type" = "Code")) %>%
  left_join(Resource, by = c("Resource" = "Resource")) %>%
  mutate(Identifier = paste0(Org,"-",Dept,"-",`IP or OP`)) %>%
  mutate(CPT = paste0(AlternateCode ,Modifier)) %>%
  left_join(Premier_Dep) %>%
  filter(!is.na(DepID)) %>%
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
MSQ_RIS_OR_upload <- MSQ_RIS_OR %>%
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
MSQ_RIS_cpt4_upload <- MSQ_RIS_cpt4 %>%
  group_by(Partner,Hosp,DepID,Start,End,CPT) %>%
  summarise(Volume = n()) %>%
  mutate(Budget = "0")

#combine upload files
MSQ_upload <- rbind(MSQ_RIS_cpt4_upload, MSQ_RIS_OR_upload) %>%
  mutate(
    Start = paste0(
      substr(Start,6,7),"/",
      substr(Start,9,10),"/",
      substr(Start,1,4)),
    End = paste0(
      substr(End,6,7),"/",
      substr(End,9,10),"/",
      substr(End,1,4)))

##################need to create master append code  
old_MSQ_master <- readRDS(paste0(MSHQ_RIS_dir,"Master/MSQ/MSQ_Master.rds"))
if(max(mdy(old_MSQ_master$End)) < min(mdy(MSQ_upload$Start))){
  new_MSQ_master <- rbind(as.data.frame(old_MSQ_master),
                      as.data.frame(MSQ_upload))
} else {
  stop("Raw data overlaps with master")
}
saveRDS(new_MSQ_master,paste0(MSHQ_RIS_dir,"Master/MSQ/MSQ_Master.rds"))
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
saveRDS(MSQ_trend, paste0(MSHQ_RIS_dir,"Master/MSQ/MSQ_Master_Trend.rds"))

#save upload
write.table(MSQ_upload,paste0(MSHQ_RIS_dir,"Uploads/MSQ/MSQ_RIS_",month_year,".csv"),
            sep = ",", row.names = F, col.names = F)
