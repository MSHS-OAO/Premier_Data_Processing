# --------RETU Observation Days
# Sums the # of observation days by year and month

# load in packages
library(dplyr)
library(readxl)

RETUdays <- function(start, pp1, pp2, pp3 = "1/1/2050"){
  # select most recent raw RETU Visits file 
  RETU <- read_excel(file.choose())
  
  # calculate the number of days
  RETU <- RETU %>%
    mutate(DAYS = difftime(`RETU DISCHARGE TIME`,
                           `RETU ROOMED (ARRIVAL) TIME`, units = "days"))
  
  # Convert data parameters to correct format
  start <- as.POSIXct(as.character(start), format = "%m/%d/%Y")
  pp1 <- as.POSIXct(as.character(pp1), format = "%m/%d/%Y")
  pp2 <- as.POSIXct(as.character(pp2), format = "%m/%d/%Y")
  pp3 <- as.POSIXct(as.character(pp3), format = "%m/%d/%Y")
  
  # Filter RETU file for both pay periods
  RETU1 <- filter(RETU, `RETU ROOMED (ARRIVAL) TIME` >= 
                    start ,`RETU ROOMED (ARRIVAL) TIME` <= pp1,)  
  RETU2 <- filter(RETU, `RETU ROOMED (ARRIVAL) TIME` > 
                    pp1 & `RETU ROOMED (ARRIVAL) TIME` <= pp2,)   
  RETU3 <- filter(RETU, `RETU ROOMED (ARRIVAL) TIME` > 
                    pp2 & `RETU ROOMED (ARRIVAL) TIME` <= pp3,) 
  
  # Get patient days for both pay periods
  pp1days <<- sum(RETU1$DAYS)
  pp2days <<- sum(RETU2$DAYS)
  pp3days <<- sum(RETU3$DAYS)
}

# start is the first day of the first pp. pp1 is end date of first pp. 
# pp2 is end date of second pp
RETUdays(start="04/23/2023", pp1="05/06/2023", pp2="05/20/2023")

# Outputs pateint day totals for pp1 and pp2
pp1days
pp2days
pp3days
