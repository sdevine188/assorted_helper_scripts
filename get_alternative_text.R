library(tidyverse)
library(rlang)


# this function takes a table and outputs concatenated alternative text that can be pasted into adobe pdf
# to provide a parsable version of the data behind image figures/plots/maps etc for 508 compliance

# https://www.section508.gov/create/pdfs
# https://accessibility.psu.edu/tableshtml/


#///////////////////////////////////////////////////////////////////////////////////////////////


# create get_alternative_text()
get_alternative_text <- function(table) {
        
        return(table %>% nest(data = everything()) %>% pmap_dfr(.l = ., .f = compile_alt_text_for_each_row))
                
}


# create compile_alt_text_for_each_row()
compile_alt_text_for_each_row <- function(data, ...) {
        
        # get row_text
        row_var_name <- data %>% select(1) %>% names() 
        row_var_text <- data %>% pull(!!sym(row_var_name))
        row_text <- str_c(row_var_name, ": ", row_var_text)
        
        return(data %>% select(-1) %>% map2(.x = ., .y = data %>% select(-1) %>% names(), 
                                     .f = ~ str_c(row_text, ", ", .y, ": ", case_when(is.na(.x) ~ "NA", TRUE ~ as.character(.x)), ";")) %>%
                bind_cols() %>% pivot_longer(cols = everything(), names_to = "var", values_to = "value") %>%
                rename("alternative text for figure:" = value) %>% select("alternative text for figure:"))
}

        
#///////////////////////////////////////////////////////////////////////////////////////////////


# test
# starwars %>% select(name, homeworld, hair_color, height, mass)
# starwars %>% select(name, homeworld, hair_color, height, mass) %>% get_alternative_text()
