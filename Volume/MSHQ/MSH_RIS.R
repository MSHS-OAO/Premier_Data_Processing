library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)

#MSH RIS directory
RIS_dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                  "Productivity/Volume - Data/MSH Data/RIS/")

#month and year of charge detail
month_year <- "MAY2022"
#Diagnostic OR scaling factor
diagnostic_scaling <- 2.69

#read in charge detail
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

#remove whitespaces
RIS[,1:21] <- sapply(RIS[,1:21], trimws)
RIS_neuro[,1:21] <- sapply(RIS_neuro[,1:21], trimws)

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
upload <- rbind(RIS_charge, neuro, RIS_OR_upload) %>%
  group_by(Partner, Hosp, DepID, Start, End, CPT4) %>%
  summarise(volume = n()) %>%
  mutate(budget = "0",
         volume = as.numeric(volume))
upload <- upload %>%
  mutate(volume = case_when(
    DepID == "MSHRIS21008OR" ~ volume * diagnostic_scaling,
    TRUE ~ volume))

####################################################  
old_master <- readRDS(paste0(RIS_dir,"Master/Master.rds"))
if(max(mdy(old_master$End)) < min(mdy(upload$Start))){
  new_master <- rbind(as.data.frame(old_master),
                      as.data.frame(upload))
} else {
  stop("Raw data overlaps with master")
}
saveRDS(new_master,paste0(RIS_dir,"Master/Master.rds"))
####################################################

#Trend Check
#Getting quarters from dates
new_master$qrts <- quarters(mdy(new_master$End))

#Creates a trend of the master biweekly to verify data is accurate
trend <- new_master %>%
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

View(trend)

#Save master trend
saveRDS(trend, paste0(RIS_dir,"Master/Master_Trend.rds"))

#save upload
write.table(upload,paste0(RIS_dir,"Uploads/MSH_RIS_",month_year,".csv"),
            sep = ",", row.names = F, col.names = F)
