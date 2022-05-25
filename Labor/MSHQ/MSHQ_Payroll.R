#MSHQ Payroll

##Libraries and Functions######################################################
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(xlsx)

#Read most recent MSHQ payroll file, filter on dates and format columns
labor <- function(start,end){
  #read paycode mapping file
  paycode <- read_xlsx(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                              "Productivity/Universal Data/Mapping/",
                              "MSHS_Paycode_Mapping.xlsx")) %>% 
    select(RAW.PAY.CODE,PAY.CODE)
  #read in most recent oracle text file and filter on start and end dates
  df <- file.info(list.files(paste0("C:/Users/lenang01/Documents/",
                                    "FTE-Projections-Dashboard/",
                                    "Raw Data/MSHQ Oracle"), full.names = T))
  df <- read.csv(rownames(df)[which.max(df$mtime)], header = T, sep = "~",
                 stringsAsFactors = F, colClasses = rep("character", 32)) %>%
    filter(as.Date(End.Date, format = "%m/%d/%Y") <= 
             as.Date(end, format = "%m/%d/%Y"),
           as.Date(Start.Date, format = "%m/%d/%Y") >= 
             as.Date(start, format = "%m/%d/%Y"),
           !is.na(Job.Code))
  #format paycode, employee name, Department names, and home department ID
  df <- left_join(df,paycode,by=c("Pay.Code"="RAW.PAY.CODE"))
  if(nrow(filter(df, is.na(PAY.CODE))) == 0){
    df <- df %>%
      mutate(Pay.Code = PAY.CODE,
             PAY.CODE = NULL,
             Employee.Name = substr(Employee.Name,1,30),
             Department.Name.Worked.Dept = substr(Department.Name.Worked.Dept,
                                                  1,50),
             Department.Name.Home.Dept = substr(Department.Name.Home.Dept,
                                                1,50),
             Department.ID.Home.Department = paste0(substr(Full.COA.for.Home,
                                                           1,3),
                                                    substr(Full.COA.for.Home,
                                                           41,44),
                                                    substr(Full.COA.for.Home,
                                                           5,7),
                                                    substr(Full.COA.for.Home,
                                                           12,16)))
  } else {
    new_paycodes <- df %>% filter(is.na(PAY.CODE))
    return(new_paycodes)
    stop("New Paycode(s). Check df for new paycode(s)")
  }
  return(df)
}
#Create JCdict and find new job codes
jcdict <- function(end){
  #read current job code mapping file
  jobcode <- read_xlsx(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                              "Productivity/Universal Data/Mapping/",
                              "MSHS_Jobcode_Mapping.xlsx"))
  jobcode <- jobcode %>% filter(PAYROLL == "MSHQ")
  df <- left_join(df,jobcode,by=c("Job.Code"="J.C"))
  #create data frame with job codes not found in job code mapping file
  newjc <- filter(df,is.na(J.C.DESCRIPTION))
  #check for new job codes
  if(nrow(newjc) > 0){
    #if new job codes then place in new job codes folder and update job code mapping file
    newjc <- newjc %>% select(Job.Code,Position.Code.Description) %>% distinct() 
    write.xlsx(newjc,paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                            "Productivity/Labor - Data/MSH/Payroll/MSH Labor/",
                            "Calculation Worksheets/NewJC/New_Job_Codes_",
                            Sys.Date(),".xlsx"))
    message(paste0("There are new Job Codes that need to be added to the Job",
                   "Code mappings File J:/deans/Presidents/SixSigma/",
                   "MSHS Productivity/Productivity/Universal Data/Mapping/",
                   "MSHS_Jobcode_Mapping.xlsx"))
  } else {
    #if no job codes then remove providers and format job code description
    df <- df %>%
      filter(PROVIDER == 0) %>%
      mutate(Position.Code.Description = substr(Position.Code.Description,1,50),
             Job.Code = substr(Job.Code,1,10))
  }
  #create job code dictionary
  jcdict <- df %>%
    select(PartnerOR.Health.System.ID, Home.FacilityOR.Hospital.ID,
           Department.IdWHERE.Worked, Job.Code, Position.Code.Description) %>%
    distinct()
  mon <- toupper(month.abb[month(as.Date(end,format = "%m/%d/%Y"))])
  #save new job code dictionary
  write.table(jcdict,paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Labor - Data/MSH/Payroll/MSH Labor/Calculation Worksheets/JCDict/MSHQ_JCdict_",substr(end,4,5),mon,substr(end,7,11),".csv"),sep=",",row.names = F,col.names = F)
  return(df)
}
#Create Department dict
depdict <- function(end){
  #take all home and worked departments and form department dictionaries
  home <- df %>% 
    select(PartnerOR.Health.System.ID, Home.FacilityOR.Hospital.ID,
           Department.ID.Home.Department,Department.Name.Home.Dept)
  worked <- df %>% 
    select(PartnerOR.Health.System.ID, Facility.Hospital.Id_Worked,
           Department.IdWHERE.Worked,Department.Name.Worked.Dept)
  col <- c("Partner","Hosp","CC","CC.Description")
  colnames(home) <- col
  colnames(worked) <- col
  #combine home and worked department dictionaries and remove duplicates
  depdict <- rbind(home,worked) %>% 
    mutate(CC.Description = substr(CC.Description,1,50)) %>%
    distinct()
  mon <- toupper(month.abb[month(as.Date(end,format = "%m/%d/%Y"))])
  #save department dictionary
  write.table(depdict,paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                             "Productivity/Labor - Data/MSH/Payroll/MSH Labor/",
                             "Calculation Worksheets/DepDict/MSHQ_DepDict_",
                             substr(end,4,5),mon,substr(end,7,11),".csv"),
              sep=",",row.names = F,col.names = F)
}
#Creates Department Mapping file for new departments
depmap <- function(end){
  #read most recent department mapping download
  depmap <- file.info(list.files(paste0("J:/deans/Presidents/SixSigma/",
                                        "MSHS Productivity/Productivity/",
                                        "Labor - Data/MSH/Payroll/MSH Labor/",
                                        "Dep Mapping Downloads"), 
                                        full.names = T,pattern = ".csv"))
  depmap <- read.csv(rownames(depmap)[which.max(depmap$mtime)], 
                     header = F,stringsAsFactors = F) %>% 
    distinct()
  #leftjoin formatted raw file with department mapping file
  depmap <- left_join(df,depmap,by=c("Department.IdWHERE.Worked"="V3")) %>%
    mutate(Effective = "01012010")
  #place any unmapped departments in a dataframe
  newdep <- depmap %>% 
    filter(is.na(V5)) %>% 
    select(Effective, PartnerOR.Health.System.ID, Facility.Hospital.Id_Worked,
           Department.IdWHERE.Worked,V5) %>% 
    distinct()
  if(nrow(newdep) > 0){
    #if new departments then create mapping file
    newdep <- newdep %>% 
      mutate(V5 = "10095")
    depmap <- depmap %>% 
      filter(!is.na(V5)) %>% 
      select(Effective, PartnerOR.Health.System.ID, Facility.Hospital.Id_Worked,
             Department.IdWHERE.Worked,V5) %>% 
      distinct()
    #combine all previously mapped departments and newly mapped departments
    depmap <- rbind(depmap,newdep)
  } else {
    #if no new departments create mapping file with current department mappings
    depmap <- depmap %>% 
      filter(!is.na(V5)) %>% 
      select(Effective, PartnerOR.Health.System.ID, Facility.Hospital.Id_Worked,
             Department.IdWHERE.Worked,V5) %>% 
      distinct() 
  }
  mon <- toupper(month.abb[month(as.Date(end,format = "%m/%d/%Y"))])
  #save new department mappign file
  write.table(depmap,paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                            "Productivity/Labor - Data/MSH/Payroll/MSH Labor/",
                            "Calculation Worksheets/DepMap/MSHQ_DepMap_",
                            substr(end,4,5),mon,substr(end,7,11),".csv")
              ,sep=",",row.names = F,col.names = F)
  return(depmap)
}
#Create JC mapping file
jcmap <- function(end){
  #join formatted labor file with new department mapping
  jcmap <- left_join(
    df,depmap,by=c("Department.IdWHERE.Worked"="Department.IdWHERE.Worked")) 
  #check again for unmapped departments
  newdep <- filter(jcmap,is.na(V5)) 
  if(nrow(newdep) > 0){
    #if still unmapped departments, tell user the department mappings was not updated correctly
    message("Deparment mapping was not updated correctly")
  }
  #if everything is mapped then create jobcode mapping file
  jcmap <- jcmap %>% 
    select(Effective, PartnerOR.Health.System.ID.x,
           Facility.Hospital.Id_Worked.x,Department.IdWHERE.Worked,Job.Code,V5,
           PREMIER.J.C) %>% 
    mutate(Allocation = "100") %>% 
    distinct()
  mon <- toupper(month.abb[month(as.Date(end,format = "%m/%d/%Y"))])
  #save jc mapping
  write.table(jcmap,paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                           "Productivity/Labor - Data/MSH/Payroll/MSH Labor/",
                           "Calculation Worksheets/JCmap/MSHQ_JCMap_",
                           substr(end,4,5),mon,substr(end,7,11),".csv"),
              sep=",",row.names = F,col.names = F)
}
#Create payroll upload
upload <- function(start,end){
  #create payroll upload
  payroll <- df %>%
    mutate(Approved = "0",
           Hours = round(as.numeric(Hours),2),
           Expense = round(as.numeric(Expense),2)) %>% 
    group_by(PartnerOR.Health.System.ID, Home.FacilityOR.Hospital.ID,
             Department.ID.Home.Department, Facility.Hospital.Id_Worked,
             Department.IdWHERE.Worked, Start.Date, End.Date, Employee.ID,
             Employee.Name,Approved,Job.Code,Pay.Code) %>%
    summarise(Hours = sum(Hours,na.rm = T),
              Expense = sum(Expense,na.rm = T))
  return(payroll)
}
#Trend worked Hours by cost center
worktrend <- function(){
  #read in paycycle calander
  paycycle <- read_xlsx(paste0("J:/deans/Presidents/SixSigma/",
                               "MSHS Productivity/Productivity/Universal Data/",
                               "Mapping/MSHS_Pay_Cycle.xlsx")) %>%
    select(DATE, END.DATE) %>%
    mutate(DATE = as.Date(DATE),
           END.DATE = as.Date(END.DATE))
  #read in paycode mapping file
  paycode <- read_xlsx(paste0("J:/deans/Presidents/SixSigma/",
                               "MSHS Productivity/Productivity/Universal Data/",
                               "Mapping/MSHS_Paycode_Mapping.xlsx")) %>%
    select(RAW.PAY.CODE, PAY.CODE.CATEGORY, INCLUDE.HOURS, INCLUDE.EXPENSES) 
  trend <- payroll %>% 
    ungroup() %>% 
    mutate(End.Date = as.Date(End.Date,format="%m/%d/%Y"))
  #bring in pay period end date to prepared upload file
  trend <- left_join(trend,paycycle,by=c("End.Date"="DATE"))
  #bring in paycode mappings to prepared upload file
  trend <- left_join(trend,paycode,by=c("Pay.Code"="RAW.PAY.CODE")) 
  trend <- trend %>%
    #filter on productive and included hours
    filter(PAY.CODE.CATEGORY %in% c("REGULAR","OTHER_WORKED","OVETIME"),
           INCLUDE.HOURS == 1) %>%
    group_by(Department.IdWHERE.Worked,END.DATE) %>%
    #summarise hours by worked department by pay period end date
    summarise(Hours = sum(Hours,na.rm=T)) %>%
    rename(PP.END.DATE = END.DATE)
  #read in old trend
  oldtrend <- readRDS(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                             "Productivity/Labor - Data/MSH/Payroll/MSH Labor/",
                             "Calculation Worksheets/Worked Trend/trend.RDS"))
  oldtrend <- mutate(oldtrend,
                     PP.END.DATE = as.Date(PP.END.DATE,formate="%Y-%m-%d"))
  #append new trended data to oldtrend
  if(max(oldtrend$PP.END.DATE) < min(trend$PP.END.DATE)){
    trend <- rbind(oldtrend,trend) %>%
      arrange(PP.END.DATE) %>%
      mutate(PP.END.DATE = factor(PP.END.DATE))
    trend <<- trend
  } else {
    stop("data overlaps with old master")
  }
  #create worked FTE trend table with newly appended data
  new_trend <- trend %>% 
    pivot_wider(id_cols = Department.IdWHERE.Worked,
                names_from = PP.END.DATE,
                values_from = Hours)
  return(new_trend)
}
#Save payroll file
save_payroll <- function(start,end){
  #establish dates for saving files
  smon <- toupper(month.abb[month(as.Date(start,format = "%m/%d/%Y"))])
  emon <- toupper(month.abb[month(as.Date(end,format = "%m/%d/%Y"))])
  mon <- toupper(month.abb[month(as.Date(end,format = "%m/%d/%Y"))])
  #save payroll upload
  write.table(payroll,paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                             "Productivity/Labor - Data/MSH/Payroll/MSH Labor/",
                             "Calculation Worksheets/Uploads/MSHQ_Payroll_",
                             substr(start,4,5),smon,substr(start,7,11)," to ",
                             substr(end,4,5),emon,substr(end,7,11),".csv"),
              sep=",",row.names = F,col.names = F)
  #save trend data in RDS form
  saveRDS(trend,paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                      "Productivity/Labor - Data/MSH/Payroll/MSH Labor/",
                      "Calculation Worksheets/Worked Trend/trend.RDS"))
  #save pivoted trend table as .csv
  write.table(new_trend,paste0("J:/deans/Presidents/SixSigma/",
                               "MSHS Productivity/Productivity/Labor - Data/",
                               "MSH/Payroll/MSH Labor/Calculation Worksheets/",
                               "Worked Trend/CC Worked Trend_",
                               substr(end,4,5),mon,substr(end,7,11),".csv"),
              sep=",",row.names = F,col.names = T)
}
###############################################################################

#Enter start and end date needed for payroll upload
start <- "03/27/2022" 
end <- "04/23/2022"
df <- labor(start,end)
#If you need to update jobcode list for new jobcodes leave R and do that in excel
#"J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Useful Tools & Templates/Job Code Mappings/MSH MSQ Position Mappings.xlsx"
df <- jcdict(end)
depdict(end)
#Download and place department mapping file in MSH Labor folder
depmap <- depmap(end)
jcmap(end)
payroll <- upload(start,end)
new_trend <- worktrend()
#If new_trend looks good then save upload
save_payroll(start,end)
