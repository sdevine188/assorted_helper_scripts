is_even <- function(x) {
        if(x %% 2 == 0){
                TRUE
        } else {
                FALSE
        }
}

str_locate_start_end_df <- function(location_unlist) {
        
        # create placeholder output df
        location_start <- c()
        location_end <- c()
        
        for(location_number in 1:length(location_unlist)) {
                if(is_even(location_number)) {
                        location_end <- c(location_end, location_unlist[location_number])
                } 
                if(!(is_even(location_number))) {
                        location_start <- c(location_start, location_unlist[location_number])
                }
        }
        
        data.frame(location_start = location_start, location_end = location_end)
}


##############################################


# test example
# test_text <- "this is a test, i hope it passes the test, the test is over"
# location <- str_locate_all(test_text, "test")
# location
# location_unlist <- unlist(location)
# location_unlist 
# str_locate_start_end_df(location_unlist)
