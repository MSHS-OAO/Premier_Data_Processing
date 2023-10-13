#MSQ Census Days

library(dplyr)
library(tidyr)
library(lubridate)

upload <- function(){
  #read raw file, skip 3 rows, set column types on read
  raw1 <- readxl::read_excel(file.choose(),skip = 3,col_types = c("text","text","text","date","numeric"))
  #remove last row (subtotal row)
  raw1 <- raw1 %>% filter(!is.na(CensusDate))
  raw1 <<- raw1
  volumeID <- read.csv("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSQ Data/Patient Days/Calculation Files/VolumeID.csv"
                       ,stringsAsFactors = F,colClasses = c(rep("character",3)))
  export <- raw1 %>%
    select(3:5) %>%
    #Bring in cost center and volID for nursing units
    left_join(volumeID,by=c("DepartmentName"="DepartmentName")) %>%
    mutate(End = format(max(CensusDate),"%m/%d/%Y"),
           Start = format(min(CensusDate),"%m/%d/%Y"),
           Partner = "729805",
           Hosp = "NY0014") %>%
    #remove units we do not upload into premier
    filter(!is.na(CC)) %>%
    group_by(Partner,Hosp,CC,Start,End,VolID) %>%
    #get sum of census over the total pay period by unit
    summarise(Census = sum(`Census Days`,na.rm = T)) %>%
    ungroup() %>%
    mutate(Budget = "0")
  colnames(export) <- c("Corporation Code", "Entity Code", "Cost Center Code",
                        "Start Date", "End Date", "Volume Code", "Actual Code",
                        "Budget Volume")
  export <<- export
}

trend <- function(){
  volumeID <- read.csv("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSQ Data/Patient Days/Calculation Files/VolumeID.csv"
                       ,stringsAsFactors = F,colClasses = c(rep("character",3)))
  #read in old master
  master_old <- readRDS("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSQ Data/Patient Days/Calculation Files/master/master.rds")
  #read in old trend file
  cols <- read.csv("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSQ Data/Patient Days/Calculation Files/trend/Census_trend.csv")
  trend_old <- read.csv("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSQ Data/Patient Days/Calculation Files/trend/Census_trend.csv",
                        stringsAsFactors = F,colClasses = c(rep("character",3),rep("numeric",ncol(cols)-3)),check.names = F)
  #bind old master with raw file to create new master
  if(max(master_old$CensusDate) < min(as.Date(raw1$CensusDate,format="%m/%d/%Y"))){
    colnames(raw1) <- colnames(master_old)
    master_new <- rbind(raw1,master_old)
    master_new <<- master_new
    #pivot old trend to long to prepare for appending current export data
    trend_old <- trend_old %>% 
      pivot_longer(cols = 4:ncol(trend_old),names_to = "End",values_to = "Census")
    colnames(trend_old) <- c("DepartmentName", "CC", "VolID", "End Date", "Actual Code")
    #prepare export data for appending to old trend
    trend_new <- volumeID %>% 
      left_join(export,by = c("CC" = "Cost Center Code",
                              "VolID" = "Volume Code")) %>%
      select(DepartmentName, `CC`, `VolID`, `End Date`,
             `Actual Code`) 
    trend_new <- do.call("rbind", list(trend_old, trend_new)) %>%
      pivot_wider(id_cols = c(DepartmentName,CC,VolID),names_from = `End Date`,values_from = `Actual Code`)
    trend_new <<- trend_new
  } else {
    stop("Raw file overlaps with master")
  }
}

save <- function(){
  saveRDS(master_new,"J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSQ Data/Patient Days/Calculation Files/master/master.rds")
  write.table(trend_new,"J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSQ Data/Patient Days/Calculation Files/trend/Census_trend.csv",
              sep = ",", row.names = F)
  #create file name
  name <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/",
                 "Volume - Data/MSQ Data/Patient Days/Calculation Files/",
                 "Uploads/MSQ_Census Days_",
                 min(as.Date(export$`Start Date`, format = "%m/%d/%Y")),"_",
                 max(as.Date(export$`End Date`, format = "%m/%d/%Y")),".csv")
  #save export in MSQ uploads folder
  write.table(export,file=name,sep=",",col.names=F,row.names=F)
}


#Date in raw file title should be ~2 days after PP end date
upload()
trend()
#review trend_new for any variance
save()

#remove variables
rm(export,master_new,raw1,trend_new)
