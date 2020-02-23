# # load fy()
# current_wd <- getwd()
# setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/helper_scripts")
# source("fy.R")
# setwd(current_wd)


library(tidyverse)
library(lubridate)

# create fy function to extract fiscal year from date
fy <- function(date) {
        
        # get month
        month <- month(date)
        
        # get year
        year <- year(date)
        
        # get and return fy
        fy <- ifelse(month < 10, year, year + 1)
        return(fy)
}


#####################


# test
# date_1 <- mdy("10-01-2012")
# date_2 <- mdy("12-31-2012")
# date_3 <- mdy("01-01-2013")
# date_4 <- mdy("04-01-2013")
# date_5 <- mdy("09-30-2013")
# 
# class(date_1)
# 
# fy(date_1)
# fy(date_2)
# fy(date_3)
# fy(date_4)
# fy(date_5)

