#--------------------Census Days For PP
library(dplyr)
library(tidyr)
library(readxl)
library(zoo)
library(lubridate)
library(DBI)
library(odbc)

#--------------raw
raw <- function(){
  #read raw file
  raw <- read_xlsx(file.choose(),skip = 4)
  census <- raw %>%
    #remove duplicat KP7
    select(-KP7...41) %>%
    rename(KP7 = KP7...51) %>%
    #remove NA dates and total row
    filter(!is.na(Date),
           Date != "Total") %>%
    #mutate dates into data format
    mutate(Date = as.numeric(Date),
           Weekday = as.numeric(Weekday)) %>%
    mutate(Date = as.Date(Date, origin = "1899-12-30"),
           Weekday = as.Date(Weekday, origin = "1899-12-30")) 
  #pivot table into longer format
  census <- census %>%
    pivot_longer(names_to = "DepID", cols = 3:ncol(census), values_to = "Census")
  
  ###add check for dates to make sure whole month###
  
  return(census)
}

#------------volumeID
volumeID <- function(){
  #read volume ID mapping file
  volID <- read.csv("/SharedDrive/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSH Data/Inpatient Census Days/New Calculation Worksheets/volID.csv",
                    stringsAsFactors = F,colClasses = c(rep("character",6)))
  #read paycycle calendar
  con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")
  paycycle <- tbl(con_prod, "LPM_MAPPING_PAYCYCLE") %>%
    select(PAYCYCLE_DATE,PP_START_DATE,PP_END_DATE) %>% 
    collect() 
  colnames(paycycle) <- c("Date", "Start.Date", "End.Date")
  paycycle <- paycycle %>%
    mutate(Date = as.Date(Date),
           Start.Date = as.Date(Start.Date),
           End.Date = as.Date(End.Date))
  #join volID mapping and paycycle with raw file
  census_vol <- left_join(census,volID,by=c("DepID"="UNIT")) %>%
    left_join(paycycle,by=("Date"="Date"))
  
  return(census_vol)
}

#------------master
master <- function(){
  #read master file
  master_old <- readRDS("/SharedDrive/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSH Data/Inpatient Census Days/New Calculation Worksheets/Master.RDS")
  #check that max master is less than min of raw file
  if(max(master_old$Date) < min(census_vol$Date) & nrow(census_vol[is.na(census_vol$End.Date),]) == 0){
    #bind raw file with old master
    master_new <- rbind(master_old,census_vol)
    #save new master
    saveRDS(master_new,"/SharedDrive/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSH Data/Inpatient Census Days/New Calculation Worksheets/Master.RDS")
  }
  #pivot new master to trend by pay period
  trend <- master_new %>% 
    ungroup()%>%
    group_by(Cost.Center, Oracle, UNIT.DESCRIPTION, End.Date) %>%
    summarise(Census = sum(Census, na.rm = T)) %>%
    pivot_wider(id_cols = c(Cost.Center, Oracle, UNIT.DESCRIPTION),names_from = End.Date,values_from = Census)
  
  trend <- trend[,c(1:3, (ncol(trend)-15):ncol(trend))]
  return(trend)
}

#------------upload
upload <- function(){
  #read new master
  master_new <- readRDS("/SharedDrive/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSH Data/Inpatient Census Days/New Calculation Worksheets/Master.RDS")
  #create upload format
  upload <- master_new %>%
    #filter on user inputed dates. remove unmapped units
    filter(Start.Date >= as.Date(start,format = "%m/%d/%Y"),
           End.Date <= as.Date(end,format = "%m/%d/%Y"),
           !is.na(Oracle)) %>%
    group_by(Oracle,OracleVol,Start.Date,End.Date) %>%
    summarise(Census = sum(Census, na.rm = T)) %>%
    mutate(Partner = "729805",
           Hosp = "NY0014",
           Budget = "0") %>%
    select(Partner,Hosp,Oracle,Start.Date,End.Date,OracleVol,Census,Budget) %>%
    ungroup() %>%
    mutate(Start.Date = format(Start.Date,format = "%m/%d/%Y"),
           End.Date = format(End.Date,format = "%m/%d/%Y"))
  colnames(upload) <- c("Corporation Code", "Entity Code", "Cost Center Code",
                        "Start Date", "End Date", "Volume Code", "Actual Volume",
                        "Budget Volume")
  return(upload)
}

#------------save
save <- function(){
  
  name <- paste0("/SharedDrive/deans/Presidents/SixSigma/MSHS Productivity/Productivity/",
                 "Volume - Data/MSH Data/Inpatient Census Days/",
                 "New Calculation Worksheets/Uploads/MSH_Census Days_",
                 as.Date(start, format = "%m/%d/%Y"), "_", 
                 as.Date(end, format = "%m/%d/%Y"), ".csv")
  write.table(census_export,name,col.names = T,row.names = F,sep = ",")
}

##################################################################
#function execution

census <- raw()
census_vol <- volumeID()
trend <- master()
#visually check/validate the trend
#Upload multiple files if necessary

#start and end should be start and end of what you want to upload
start = "03/24/2024"
end = "04/20/2024"
census_export <- upload()
save()
