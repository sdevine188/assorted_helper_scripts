library(tidyverse)

# add months to date by incrementing the months digit (disregarding length of months)

# create add_months function
add_months <- function(date, months_to_add) {
        new_date <- date
        month(new_date) <- month(new_date) + months_to_add
        return(new_date)
}


##############################


# test add_months
data <- tibble(date = ymd("2014-02-15"))
data
data %>% pull(date) %>% add_months(date = , months_to_add = 1:12)


################################


# Add and subtract months to a date without exceeding the last day of the new month
# https://stackoverflow.com/questions/22628863/add-subtract-6-months-bond-time-in-r-using-lubridate
as.Date("2014-12-31") %m+% months(1)
as.Date("2014-12-31") %m+% months(6)

as.Date("2014-12-15") %m+% months(1)
as.Date("2014-12-15") %m+% months(6)




