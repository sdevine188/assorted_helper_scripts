# # load run_loop()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("run_loop.R")
# setwd(current_wd)


library(tidyverse)
library(KeyboardSimulator)


# create run_loop function

# create click_cursor function
click_cursor <- function(current_minute) {
        
        # click mouse
        mouse.click()

        # wait a minute
        Sys.sleep(time = 60)
        
        # print minutes elapsed
        print(current_minute)
}

# create run_loop function
run_loop <- function(minutes = 60) {
        
        # call move_cursor 
        walk(.x = seq(from = 1, to = minutes, by = 1), .f = ~ click_cursor(current_minute = .x))
}


#####################


# test
# run_loop(minutes = 5)
# run_loop()

