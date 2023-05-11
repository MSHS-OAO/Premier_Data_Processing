library(readxl)
library(here)
library(tidyverse)
library(xlsx)
library(lubridate)
library(dplyr)

#MSQ RIS directory
RIS_dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                  "Productivity/Volume - Data/MSQ Data/RIS/")

#month and year of charge detail
month_year <- "JUL2022"

#read in charge detail
RIS <- read.csv(paste0(RIS_dir,"Charge Detail/",
                       "MSQ_RIS_",month_year,".csv"), 
                colClasses = c(rep("character",21)))   
#read in OR Diagnostic
RIS_OR <- read.csv(paste0(RIS_dir,"Charge Detail/OR Diagnostic/",
                          "MSQ_RIS_OR Diagnostic_",month_year,".csv"), 
                   colClasses = c(rep("character",9))) %>%
  select(MRN, Name, ACC, Date, Exam, Exam.Modifier, Org, Resource)

#read in CDM
CDM <- read_xlsx(file.choose(), col_types = rep("text",21))
#read in PAT Mapping
PAT <- read_xlsx(paste0(RIS_dir,"Mapping/PAT_Mapping.xlsx"))
#read in Dep ID mapping
Resource <- read_xlsx(paste0(RIS_dir,"Mapping/Resource_Mapping.xlsx"),
                      col_types = c("text","text","text"))
#Premier Dep ID for CPT upload
Premier_Dep <- read_xlsx(paste0(RIS_dir,"Mapping/Premier_Dep.xlsx"))

#Read in pay period mapping for trend
pp_mapping <- read_xlsx(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                               "Productivity/Universal Data/Mapping/",
                               "MSHS_Pay_Cycle.xlsx"))

#Read in department mapping for trend
dept_mapping <- read_xlsx(paste0(RIS_dir,"/Mapping/",
                                 "Dep_Description_Mapping.xlsx"))

#Read in CPT reference for trend mapping and selects columns needed
cpt_mapping <- read_xlsx(paste0(RIS_dir,"/Mapping/",
                                "CPT_Ref.xlsx")) %>%
  select(1, 2, 3, 6, 12)

#remove whitespaces
RIS[,1:21] <- sapply(RIS[,1:21], trimws)
#pivot charge columns longer into a single column
RIS_charge <- RIS %>% 
  pivot_longer(cols = c(10:17), names_to = "Charge", values_to = "charge_code") %>%
  filter(charge_code != "") %>%
  select(-Charge)
#modifiers we want to keep
acceptable_modifiers <- c("26","50","53","tc")
#remove unwanted modifiers and concatenate remaining
RIS_charge_mod <- RIS_charge %>%
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
RIS_cpt4 <- left_join(RIS_charge_mod, CDM_join) %>%
  filter(!is.na(AlternateCode)) %>%
  left_join(PAT, by = c("Pat.Type" = "Code")) %>%
  left_join(Resource, by = c("Resource" = "Resource")) %>%
  mutate(Identifier = paste0(Org,"-",Dept,"-",`IP or OP`)) %>%
  mutate(CPT = paste0(AlternateCode ,Modifier)) %>%
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
RIS_OR_upload <- RIS_OR %>%
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
RIS_cpt4_upload <- RIS_cpt4 %>%
  group_by(Partner,Hosp,DepID,Start,End,CPT) %>%
  summarise(Volume = n()) %>%
  mutate(Budget = "0")

#combine upload files
upload <- rbind(RIS_cpt4_upload, RIS_OR_upload) %>%
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
old_master <- readRDS(paste0(RIS_dir,"Master/New/Master.rds"))
if(max(mdy(old_master$End)) < min(mdy(upload$Start))){
  new_master <- rbind(as.data.frame(old_master),
                      as.data.frame(upload))
} else {
  stop("Raw data overlaps with master")
}
saveRDS(new_master,paste0(RIS_dir,"Master/New/Master.rds"))
####################################################

#Trend Check
#Getting quarters from dates
new_master$qrts <- quarters(mdy(new_master$End))

#Creates a trend of the master biweekly to verify data is accurate
trend <- new_master %>%
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

View(trend)

#Save master trend
saveRDS(trend, paste0(RIS_dir,"Master/New/Master_Trend.rds"))

#save upload
write.table(upload,paste0(RIS_dir,"Uploads/new_2022/MSQ_RIS_",month_year,".csv"),
            sep = ",", row.names = F, col.names = F)
