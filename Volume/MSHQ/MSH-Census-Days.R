#--------------------Census Days For PP
library(dplyr)
library(tidyr)
library(readxl)
library(zoo)
library(lubridate)

#--------------raw
raw <- function(){
  #read raw file
  raw <- read_xlsx(file.choose(),skip = 4)
  census <- raw %>%
    #remove duplicat KP7
    select(-KP7...40) %>%
    rename(KP7 = KP7...50) %>%
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
  volID <- read.csv("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSH Data/Inpatient Census Days/New Calculation Worksheets/volID.csv",
                    stringsAsFactors = F,colClasses = c(rep("character",6)))
  #read paycycle calendar
  paycycle <- read_xlsx("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Mapping/MSHS_Pay_Cycle.xlsx") %>%
    select(DATE,START.DATE,END.DATE) 
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
  master_old <- readRDS("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSH Data/Inpatient Census Days/New Calculation Worksheets/Master.RDS")
  #check that max master is less than min of raw file
  if(max(master_old$Date) < min(census_vol$Date) & nrow(census_vol[is.na(census_vol$End.Date),]) == 0){
    #bind raw file with old master
    master_new <- rbind(master_old,census_vol)
    #save new master
    saveRDS(master_new,"J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSH Data/Inpatient Census Days/New Calculation Worksheets/Master.RDS")
  }
  #pivot new master to trend by pay period
  trend <- master_new %>% 
    ungroup()%>%
    group_by(Oracle,End.Date) %>%
    summarise(Census = sum(Census, na.rm = T)) %>%
    pivot_wider(id_cols = c(Oracle),names_from = End.Date,values_from = Census)
  
  return(trend)
}

#------------upload
upload <- function(start,end){
  #read new master
  master_new <- readRDS("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSH Data/Inpatient Census Days/New Calculation Worksheets/Master.RDS")
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
  return(upload)
}

#------------save
save <- function(){
  MinDate <- min(as.Date(census_export$Start.Date,format = "%m/%d/%Y"))
  MaxDate <- max(as.Date(census_export$End.Date,format = "%m/%d/%Y"))
  smonth <- toupper(month.abb[month(MinDate)])
  emonth <- toupper(month.abb[month(MaxDate)])
  sday <- format(as.Date(MinDate, format="%Y-%m-%d"), format="%d")
  eday <- format(as.Date(MaxDate, format="%Y-%m-%d"), format="%d")
  syear <- substr(MinDate, start=1, stop=4)
  eyear <- substr(MaxDate, start=1, stop=4)
  name <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSH Data/Inpatient Census Days/New Calculation Worksheets/Uploads/","MSH_Census Days_",sday,smonth,syear," to ",eday,emonth,eyear,".csv")
  write.table(census_export,name,col.names = F,row.names = F,sep = ",")
}

##################################################################
#function execution

census <- raw()
census_vol <- volumeID()
trend <- master()
#visually check/validate the trend
#Upload multiple files if necessary

#start and end should be start and end of what you want to upload
census_export <- upload(start = "01/02/2022",end = "01/29/2022")
save()
