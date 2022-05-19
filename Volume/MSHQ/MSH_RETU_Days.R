#--------RETU Observation Days
#Sums the # of observation days by year and month

RETUdays <- function(start, pp1, pp2, pp3 = "1/1/2050"){
  #select most recent raw RETU Visits file 
  RETU <- readxl::read_excel(file.choose())
  #use difftime function to calculate the number of days between two POSIXct timestamps  
  RETU <- cbind(RETU,difftime(RETU$`RETU DISCHARGE TIME`, RETU$`RETU ROOMED (ARRIVAL) TIME`, units = "days"))
  colnames(RETU)[16] <- "DAYS"
  #Convert data parameters to correct format
  start <- as.POSIXct(as.character(start), format = "%m/%d/%Y")
  pp1 <- as.POSIXct(as.character(pp1), format = "%m/%d/%Y")
  pp2 <- as.POSIXct(as.character(pp2), format = "%m/%d/%Y")
  pp3 <- as.POSIXct(as.character(pp3), format = "%m/%d/%Y")
  #Filter RETU file for both pay perids
  RETU1 <- RETU[RETU$`RETU ROOMED (ARRIVAL) TIME`>=start & RETU$`RETU ROOMED (ARRIVAL) TIME`<=pp1,]
  RETU2 <- RETU[RETU$`RETU ROOMED (ARRIVAL) TIME`>pp1 & RETU$`RETU ROOMED (ARRIVAL) TIME`<=pp2,]
  RETU3 <- RETU[RETU$`RETU ROOMED (ARRIVAL) TIME`>pp2 & RETU$`RETU ROOMED (ARRIVAL) TIME`<=pp3,]
  #Get patient days for both pay periods
  pp1days <<- sum(RETU1$DAYS)
  pp2days <<- sum(RETU2$DAYS)
  pp3days <<- sum(RETU3$DAYS)
}

#start is the first day of the first pp. pp1 is end date of first pp. pp2 is end date of second pp
RETUdays(start="02/27/2022", pp1="03/12/2022", pp2="03/26/2022")

#Outputs pateint day totals for pp1 and pp2
pp1days
pp2days
pp3days
