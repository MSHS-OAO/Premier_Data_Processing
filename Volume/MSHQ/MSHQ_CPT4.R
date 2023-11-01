#MSHQ Charges
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(xlsx)

#Bring in Rev Dep to CC crosswalk
rev_map <- read_excel("J:\\deans\\Presidents\\SixSigma\\MSHS Productivity\\Productivity\\Volume - Data\\MSH Data\\Charges\\MSHQ REV CROSSWALK.xlsx") %>%
  select(c(1,5:8)) %>%
  distinct()
#Bring in CPT reference table
#For months: January,April,July,October there is a new cpt_ref to download
# https://communities.premierinc.com/display/OUG/Data+Management%3A+Productivity+%28Legacy%29+Topics
cpt_ref <- read_excel("J:\\deans\\Presidents\\SixSigma\\MSHS Productivity\\Productivity\\Volume - Data\\MSH Data\\RIS\\Mapping\\CPT_Ref.xlsx") %>%
  select(1,2,3,6,11,12)

#Prepares file for master
charges <- function(MSH,MSQ){
  #colnames for the charge details
  chargenames <- c("SINAI.CODE","REV.DEP","DESCRIPTION","CPT","QTY","MONTH")
  colnames(MSH) <- chargenames
  colnames(MSQ) <- chargenames
  MSH <- MSH %>% 
    filter(!is.na(SINAI.CODE))
  MSQ <- MSQ %>% 
    filter(!is.na(SINAI.CODE))
  #combine MSH and MSQ charge details
  MSHQ <- rbind(MSH,MSQ) %>%
    mutate(REV.DEP = as.character(REV.DEP))
  #remove blank CPT lines
  MSHQ <- filter(MSHQ,!is.na(CPT))
  #establish what quarter of the year it is
  if(max(as.numeric(MSHQ$MONTH)) < 4){
    Q <<- 1
  } else if(max(MSHQ$MONTH) < 7){
    Q <<- 2
  } else if(max(MSHQ$MONTH) < 10){
    Q <<- 3
  } else {
    Q <<- 4
  }
  #bring in revenue center mapping
  MSHQ <- left_join(MSHQ,rev_map) %>% 
    #remove non-premier revenue centers
    filter(!is.na(ORACLE.CC)) %>%
    #begin formatting for upload
    mutate(PARTNER = "729805",
           BUDGET = "0",
           HOSP = "NY0014",
           START = as.Date(paste0(MONTH,"/1/",Year),format = "%m/%d/%Y"),
           QUARTER = Q,
           QTY = as.numeric(QTY))
  #Add start and end date and finish formatting
  MSHQ <- mutate(MSHQ,
                 END = paste0(substr(START,6,7),"/",days_in_month(START),"/",Year),
                 START = paste0(substr(START,6,7),"/",substr(START,9,10),"/",Year),
                 REP.DEFINITION = paste0(REP.ID," - ",REP.NAME)) %>%
    group_by(PARTNER,HOSP,ORACLE.CC,START,END,CPT,BUDGET,CPT.GROUP,REP.DEFINITION,QUARTER) %>%
    summarise(QTY = sum(QTY, na.rm=T)) %>%
    select(PARTNER,HOSP,ORACLE.CC,START,END,CPT,QTY,BUDGET,CPT.GROUP,REP.DEFINITION,QUARTER)
  return(MSHQ)
}
#Creates master repository and master trend
master <- function(){
  #read in master cpt file
  master <- readRDS("J:\\deans\\Presidents\\SixSigma\\MSHS Productivity\\Productivity\\Volume - Data\\MSH Data\\Charges\\Master\\master.RDS")
  #Check that new charge detail does not overlap with master
  if(max(as.Date(master$END,format = "%m/%d/%Y")) < min(as.Date(MSHQ$START,format = "%m/%d/%Y"))){
    master <- rbind.data.frame(master,MSHQ)
  } else {
    stop("Raw data overlaps with master")
  }
  #trend out the master file by month to validate data
  master_trend <- master %>%
    mutate(`Concatenate for lookup` = paste0(substr(END,7,10),"Q",QUARTER,CPT)) %>%
    left_join(.,cpt_ref) %>%
    mutate(LABOR = case_when(
      CPT.GROUP == "PROCEDURE" ~ QTY*`CPT Procedure Count`,
      CPT.GROUP == "LAB" ~ QTY*`Lab Procedure Count`,
      CPT.GROUP == "RVU" ~ QTY*`Facility Practice Expense RVU Factor`),
      LABOR = as.numeric(LABOR),
      DATE = as.Date(END, format = "%m/%d/%Y")) %>%
    filter(LABOR > 0) %>%
    group_by(REP.DEFINITION,CPT.GROUP,END,DATE)%>%
    summarise(LABOR = sum(LABOR,na.rm = T)) %>%
    arrange(DATE) %>%
    pivot_wider(id_cols = c(REP.DEFINITION,CPT.GROUP),names_from = END,values_from = LABOR)
  #save master and trend to global environment
  master <<- master
  master_trend <<- master_trend
}
#Saves master files and upload
upload_master <- function(){
  saveRDS(master,"J:\\deans\\Presidents\\SixSigma\\MSHS Productivity\\Productivity\\Volume - Data\\MSH Data\\Charges\\Master\\master.rds")
  write.xlsx(as.data.frame(master_trend),"J:\\deans\\Presidents\\SixSigma\\MSHS Productivity\\Productivity\\Volume - Data\\MSH Data\\Charges\\Master\\master_trend.xlsx",
             row.names = F)
  upload <- MSHQ %>% ungroup() %>% select(c(1:8))
  colnames(upload) <- c("Corporation Code", "Entity Code", "Cost Center Code",
                        "Start Date", "End Date", "CPT Code", "Actual Volume", 
                        "Budget Volume")
  name <- paste0("MSHQ_CPT4_", as.Date(min(MSHQ$START),format = "%m/%d/%Y"), "_", as.Date(max(MSHQ$END),format = "%m/%d/%Y"),".csv")
  upload_path <- paste0("J:\\deans\\Presidents\\SixSigma\\MSHS Productivity\\Productivity\\Volume - Data\\MSH Data\\Charges\\Uploads\\",name)
  write.table(upload,upload_path,sep = ",",row.names = F)
}

# choose path for this months data
path <- file.choose()
sheetnames <- excel_sheets(path)
mylist <- lapply(excel_sheets(path), read_excel, path = path, col_names = T)
names(mylist) <- sheetnames
# print sheets in selected file
names(mylist)

#Enter Year of data
Year <- "2023"
# tell charges function which sheet is MSH and which is MSQ
MSHQ <- charges(MSH = mylist[[1]],MSQ = mylist[[2]])
#Create master and master trend
master()
#Review master trend

#Create upload and save both master files and upload
upload_master()

