# # load run_loop()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("run_loop.R")
# setwd(current_wd)


library(tidyverse)
library(KeyboardSimulator)


# create run_loop function

# create click_cursor function
click_cursor <- function(current_second, elapsed_min) {
        
        # click mouse
        mouse.click()
        
        # wait a minute
        Sys.sleep(time = current_second)
        
        # print minutes elapsed
        print(elapsed_min)
}

# create run_loop function
run_loop <- function(minutes = 60) {
        
        # get second_tbl
        second_tbl <- sample(x = 1:120, size = c(60*60*24), replace = TRUE) %>% tibble(second = .) %>%
                mutate(elapsed_min = round(cumsum(second) / 60, digits = 1),
                       target_min = minutes,
                       time_remaining_to_target_min = target_min - elapsed_min) %>%
                filter(time_remaining_to_target_min > 0) %>%
                arrange(desc(time_remaining_to_target_min))
        second_tbl
        
        # call move_cursor 
        walk(.x = 1:nrow(second_tbl), .f = ~ click_cursor(current_second = second_tbl %>% slice(.x) %>% pull(second),
                                                          elapsed_min = second_tbl %>% slice(.x) %>% pull(elapsed_min)))
}


#####################


# test
# run_loop(minutes = 5)
# run_loop()
