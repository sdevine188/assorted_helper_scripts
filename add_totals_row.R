# # load test_means()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("add_totals_row.R")
# setwd(current_wd)



# create sum_col function to be called inside add_totals_row function
sum_col <- function(vector) {
        
        # return NA if vector is character, list, or factor
        if(class(vector) %in% c("character", "list", "factor")) { 
                return(NA)
        }
        
        # return sum if vector is numeric, integer, or logical
        if(class(vector) %in% c("numeric", "integer", "logical")) {
                return(sum(vector, na.rm = TRUE))
        }
}

# create add_totals_row function
add_totals_row <- function(tbl, position = "bottom", label = FALSE) {
        
        # call sum_col to get totals_row
        totals_row <- tbl %>% map_dfc(.f = ~ sum_col(vector = .x))
        
        # add totals label if label = TRUE
        if(label == TRUE) {
                totals_row[1, 1] <- "Total"
        }
        
        # add totals_row to top or bottom of tbl
        if(position == "bottom") {
                return(bind_rows(tbl, totals_row))
        }
        
        if(position == "top") {
                return(bind_rows(totals_row, tbl))
        }
}


####################


# test
# tbl <- starwars
# position <- "bottom"
# tbl %>% glimpse()
# 
# starwars %>% add_totals_row() %>% tail()
# starwars %>% add_totals_row(position = "top", label = TRUE) %>% head()
