library(tidyverse)
library(data.table)
library(xlsx)
library(readxl)

default_dir <- "C:\\Users\\mieslm01\\Documents\\R_projects\\RIS_Data_Processing\\Data\\"

# begin_date <- as.Date("2021/05/23")
# final_date <- as.Date("2021/06/19")


month_dir <- choose.dir(default = default_dir)

mods_OK <- c("26", "50", "53", "TC")

# read file path
all_folders <-
  list.dirs(
    path = month_dir,
    full.names = TRUE
  )

all_paths <-
  list.files(
    path = all_folders,
    pattern = "*.xls",
    full.names = TRUE
  )

# read file content
all_content <-
  all_paths %>%
  lapply(read_xls, col_types = c("guess", "guess", "guess", "guess", "guess",
                                 "guess", "guess", "guess", "guess",
                                 "text", "text", "text", "text", "text",
                                 "text", "text", "text", "text", "text",
                                 "text", "text"))

map_file_folder <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSBI Data/Charge Detail/Instruction Files/*.*"

# path_dictionary_pay_cycles <- choose.files(default = map_file_folder, caption = "Select Pay Cycles File", multi = F)
# dictionary_pay_cycles <- read_xlsx(path_dictionary_pay_cycles, sheet = 1, col_types = c("date", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "date", "date", "skip"))
# dictionary_pay_cycles$Date <- as.Date(dictionary_pay_cycles$Date)
# dictionary_pay_cycles$`Start Date` <- as.Date(dictionary_pay_cycles$`Start Date`)
# dictionary_pay_cycles$`End Date` <- as.Date(dictionary_pay_cycles$`End Date`)

CDM_file_path <- choose.files(default = map_file_folder, caption = "Select CDM File", multi = F)
CDM <- read_xlsx(CDM_file_path, sheet = 1) # , col_types = c('date', 'skip', 'skip', 'skip', 'skip','skip', 'skip','skip', 'skip','skip','date', 'date', 'skip'))
CDM_slim <- CDM %>% select(CHARGE_CODE, CHARGE_DESC, CHARGE_CLASS, OPTB_cpt4)
CDM_slim$CHARGE_CODE <- stringr::str_trim(CDM_slim$CHARGE_CODE)
CDM_slim$OPTB_cpt4[is.na(CDM_slim$OPTB_cpt4)] <- "0"
CDM_slim$CHARGE_CLASS <- as.numeric(CDM_slim$CHARGE_CLASS)

# NEED TO POINT BELOW items to the INSTRUCTIONS FILE

instruction_file_path <- choose.files(default = map_file_folder, caption = "Select Instruction File", multi = F)

# charge_class_path <- choose.files(default = map_file_folder, caption = "Select Charge Class File", multi = F)
charge_class_map <- read_xlsx(instruction_file_path, sheet = "Charge Class Mappings BIBSLR")
# charge_class_map$`Charge Class` <- paste0("0", as.character(charge_class_map$`Charge Class`))


# PAT_type_path <- choose.files(default = map_file_folder, caption = "Select PAT Type File", multi = F)
# PAT_type_map <- read_xls(PAT_type_path)

PAT_type_map <- read_xlsx(instruction_file_path, sheet = "PAT Mapping All Sites")

# cost_center_path <- choose.files(default = map_file_folder, caption = "Select Cost Center Map File", multi = F)
cost_center_map <- read_xlsx(instruction_file_path, sheet = "Identifiers to CC Map MSBIBUSCC")

# read file name
# all_filenames <- all_paths %>%
#   basename() %>%
#   as.list()

# combine file content list and file name list
# all_lists <- mapply(c, all_content, all_filenames, SIMPLIFY = FALSE)

# unlist all lists and change column name
all_result <- rbindlist(all_content, fill = T)

all_result_long <- all_result %>%
  filter(!is.na(`Charge One`)) %>%
  pivot_longer(cols = c(`Charge One`, `Charge Two`, `Charge Three`,
                        `Charge Four`, `Charge Five`, `Charge Six`,
                        `Charge Seven`, `Charge Eight`),
               names_to = "Charge_Order",
               values_to = "Charge")

all_result_long <- all_result_long %>%
  filter(!is.na(Charge)) %>%
  mutate(`Charge Mod 1` =
           replace(`Charge Mod 1`, !`Charge Mod 1` %in% mods_OK, NA ),
         `Charge Mod 2` =
           replace(`Charge Mod 2`, !`Charge Mod 2` %in% mods_OK, NA ),
         `Charge Mod 3` =
           replace(`Charge Mod 3`, !`Charge Mod 3` %in% mods_OK, NA ),
         `Charge Mod 4` =
           replace(`Charge Mod 4`, !`Charge Mod 4` %in% mods_OK, NA )) %>%
  mutate(Charge_Mod = paste0(`Charge Mod 1`, `Charge Mod 2`,
                             `Charge Mod 3`, `Charge Mod 4`))
all_result_long$Charge_Mod <- gsub("NA", "", all_result_long$Charge_Mod)
# all_result_long4$Charge_Mod <- gsub("", NA, all_result_long4$Charge_Mod)

all_result_long <- all_result_long %>%
  mutate(Charge_Mod = 
           replace(Charge_Mod, Charge_Mod == "", NA))

all_result_long <- left_join(x = all_result_long,
                             y = CDM_slim,
                             by = c("Charge" = "CHARGE_CODE"),
                             all.x = T)

# IMPORTANT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# IF A CHARGE DOESN'T HAVE A MAP, IS IT ACTUALLY A CPT4 CODE???????
# SOME appear to be valid.  But then we're missing a Charge Class...


all_result_long <- left_join(x = all_result_long,
                             y = charge_class_map,
                             by = c("CHARGE_CLASS" = "Charge Class"),
                             all.x = T)
# all_result_long <- all_result_long %>%
#   mutate(Charge_Class_Name = `Dept ID`,
#          `Dept ID` = NULL)

all_result_long <- left_join(x = all_result_long,
                             y = PAT_type_map,
                             by = c("Pat Type" = "Code"),
                             all.x = T) %>%
  mutate(PAT_desc = Description,
         Description = NULL)

##################
# Find all KHMG1 and KHMUS Resource Codes in charge class 410 (IR) and
# manual edit charge class to 420 (Mammography) for those charges

all_result_long <- 
  all_result_long %>%
   mutate(CHARGE_CLASS = case_when(
     ((Resource == "KHMG1" | Resource == "KHMUS") & CHARGE_CLASS == 410) ~ 420,
     TRUE ~ CHARGE_CLASS)) %>%
  mutate(`Charge Class Name` = case_when(
    ((Resource == "KHMG1" | Resource == "KHMUS") & CHARGE_CLASS == 420) ~ "Mammography",
    TRUE ~ `Charge Class Name`))
###################

############
# In Error Report NA mapped to MSUS , all codes that start with "99" and
# Org PH should be mapped to MSUS IR. These codes are actually CPT codes
# not charge codes
############
all_result_long <- 
  all_result_long %>%
  mutate(CHARGE_CLASS = case_when(
    (Org == "PH" & substr(Charge, 1, 2) == "99") ~ 410,
    TRUE ~ CHARGE_CLASS)) %>%
  mutate(`Charge Class Name` = case_when(
    (Org == "PH" & substr(Charge, 1, 2) == "99") ~ "Interventional Radiology",
    TRUE ~ `Charge Class Name`)) %>%
  mutate(OPTB_cpt4 = case_when(
    (Org == "PH" & substr(Charge, 1, 2) == "99") ~ Charge,
    TRUE ~ OPTB_cpt4))

all_result_long <- all_result_long %>%
  mutate(Identifier = paste0(Org, "-", CHARGE_CLASS, "-", `IP or OP`))


all_result_long <- left_join(x = all_result_long,
                             y = cost_center_map,
                             by = c("Identifier" = "Identifier"),
                             all.x = T)

all_result_long <- all_result_long %>%
  mutate(CPT4 = case_when(
    !is.na(Charge_Mod) ~ paste0(OPTB_cpt4, Charge_Mod),
    TRUE ~ OPTB_cpt4))

# check for blank CPT4 to see if charge code is in the list of CPT4
no_cpt4_charges <- all_result_long %>%
  filter(is.na(OPTB_cpt4)) %>%
  select(Charge) %>%
  unique()

valid_cpt4s <- no_cpt4_charges %>%
  filter(Charge %in% unique(CDM_slim$OPTB_cpt4))

# no_cc_check <- all_result_long %>% filter(is.na(`Cost Center`))
# 15314/58646

all_result_long <- all_result_long %>%
  mutate(Date_Only = as.Date(Date))

# mutate(Date_Only = as.Date(Date))
# filter(Date_Only >= begin_date & Date_Only <= final_date) %>%

charge_summary <- all_result_long %>%
  filter(!is.na(`Dummy Cost Center`) & `Dummy Cost Center` != "EXCLUDE" &
           CPT4 != "0" & !is.na(CPT4)) %>%
  count(`Dummy Cost Center`, Date_Only, CPT4) %>%
  rename(Vol = n)

charge_summary$EntityID <- rep(729805, nrow(charge_summary))
charge_summary$FacilID <- rep(630571, nrow(charge_summary))
charge_summary$budget <- rep(0, nrow(charge_summary))



charge_summary <- charge_summary %>%
  # filter(Date_Only >= begin_date & Date_Only <=final_date) %>%
  mutate(Date_Only = format(Date_Only, "%m/%d/%Y")) %>%
  mutate(StartDate = Date_Only,
         EndDate = Date_Only) %>%
  select(EntityID, FacilID, `Dummy Cost Center`, StartDate, EndDate, CPT4, Vol, budget)

vol_folder <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSBI Data/RIS Data"


write.table(charge_summary, file = paste0(vol_folder,"/RIS upload.csv"), row.names = F, col.names = F, sep = ",")
write.csv(all_result_long, file = paste0(vol_folder,"/RIS processed data.csv"), row.names = F, col.names = T, sep = ",")
