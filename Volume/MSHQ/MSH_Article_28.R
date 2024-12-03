#---------Art 28 Visits

#First save raw file
#Create Art 28 function
Art28 <- function(YYYYMM){
  #Read the raw excel file from Doran
  raw <- readxl::read_excel(file.choose())
  #assign column names
  colnames(raw) <- c("YearMo","Clinic Group","Clinic Group Description","Clinic Code","Attending MD","Payor Group","Encounters")
  #filter on the month and year
  raw <- raw[raw$YearMo == YYYYMM,]
  #Create vector for all departments recieving volumes
  cc <<- c(01040170,01040185,01040388,01040404,01040407,01040409,01040411,
           01040426,01040436,01040435,01040438,01040441,01040447,01040458,
           01040460,01040464,01040473,01040477)
  #Create each specific volume dataframe
  volume1 <- raw[raw$`Clinic Code` %in% c("HI196","HI198","RE930","RE994"),]
  volume2 <- raw[raw$`Clinic Code` %in% c("MH349",	"MH346"),]
  volume3 <- raw[raw$`Clinic Code` %in% c("MH348",	"MH363"),]
  volume4 <- raw
  volume5 <- raw[raw$`Clinic Code` %in% c("CL678","CL679","CL681","CL688"),]
  volume6 <- raw[raw$`Clinic Code` %in% c("CL647","CL651","CL652","CL653","CL654","CL657","CL660"),]
  volume7 <- raw[raw$`Clinic Code` %in% c("CL641","CL644","CL646","CL638"),]
  volume8 <- raw[raw$`Clinic Code` %in% c("GV475","GV476"),]
  volume9 <- raw[raw$`Clinic Code` %in% c("SB411","SB412","SB413","SB414"),]
  volume10 <- raw[raw$`Clinic Code` %in% c("CL101","CL103","CL106","CL270","CL271","CL642","CL191","CL197","CL155","CL194","CL193","CL265","CL280","CL160","CL107","CL297","CL294","CL435","CL201","CL202","CL206","CL208","CL205","CL203","CL140","CL147","CL285","CL212","CL213","PD230","PD240"),]
  volume11 <- raw[raw$`Clinic Code` %in% c("AH379","AH401","DE125","AH402","AH549"),]
  volume12 <- raw[raw$`Clinic Code` %in% c("CL194"),]
  volume13 <- raw[raw$`Clinic Code` %in% c("HE307","HE309"),]
  volume14 <- raw[raw$`Clinic Code` %in% c("DI622"),]
  volume15 <- raw[raw$`Clinic Code` %in% c("CV260"),]
  volume16 <- raw[raw$`Clinic Code` %in% c("CL188"),]
  volume17 <- raw[raw$`Clinic Code` %in% c("MH330"),]
  volume18 <- raw[raw$`Clinic Code` %in% c("ER494"),]
  #create a vecotr of each departments volume
  vol <- c(sum(volume1$Encounters),sum(volume2$Encounters),sum(volume3$Encounters),
           sum(volume4$Encounters),sum(volume5$Encounters),sum(volume6$Encounters),
           sum(volume7$Encounters),sum(volume8$Encounters),sum(volume9$Encounters),
           sum(volume10$Encounters),sum(volume11$Encounters),sum(volume12$Encounters),
           sum(volume13$Encounters),sum(volume14$Encounters),sum(volume15$Encounters),
           sum(volume16$Encounters),sum(volume17$Encounters),sum(volume18$Encounters))
  #output each department with its associated volume
  volume <<- cbind(cc,vol)
  colnames(volume) <- c("cc",YYYYMM)
  setwd("/SharedDrive/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSH Data/Art 28 Visits/Calculation Worksheet")
  #Read Master Art28 file
  master <- read.csv("Art28_Master.csv",check.names=F)
  #append master file and save it back
  master <<- merge(master,volume,by="cc")
  write.csv(master,"Art28_Master.csv",row.names=F)
}

#enter year and month in YYYYMM format
#for example 201802 would represent February 2018
Art28("202311")

