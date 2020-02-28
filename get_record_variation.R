# # load get_record_variation()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("get_record_variation.R")
# setwd(current_wd)

library(tidyverse)


# create get_record_variation function
get_record_variation <- function(data, id_vars, arrange_by = "sum") {
        
        # handle id_vars arg, converting input to strings
        
        # handle single bare variables passed as id_vars
        if(deparse(substitute(id_vars)) %in% names(data)) {
                
                id_vars <- deparse(substitute(id_vars))
                
        } else if("quosure" %in% class(id_vars) | "quosures" %in% class(id_vars)) {
                        
                # handle id_vars if it's passed using quo(), quos(), or id_vars(), including tidyselect helpers
                id_vars <- data %>% ungroup() %>% select(!!!(id_vars)) %>% names()
                
        } else if(class(id_vars) == "character") {
                                
                # handle id_vars as a string
                id_vars <- id_vars
        }
        
        
        ################################################################################################################################
        
        
        # get record_variation showing n_distinct for each record_var, for each id
        record_variation <- data %>% 
                group_by(!!!syms(id_vars)) %>% summarize_at(.vars = vars(everything()), .funs = n_distinct) %>%
                ungroup() 
        
        
        #####################
        
        
        # get record_count for each id
        record_count <- data %>% count(!!!syms(id_vars)) %>% rename(record_count = n)
        
        
        #######################
        
        
        # get n_distinct_sum for each id, providing summary measure of overall variation across all variables in the record
        n_distinct_sum <- record_variation %>% pivot_longer(col = -c(!!!syms(id_vars)), names_to = "var_name", values_to = "n_distinct") %>%
                group_by(!!!syms(id_vars)) %>% mutate(n_distinct_sum = sum(n_distinct, na.rm = TRUE)) %>% slice(1) %>% ungroup() %>% 
                select(-c(n_distinct, var_name)) 
        
        
        
        ########################
        
        # get n_distinct_max for each id, providing summary measure of the maximum variation for any one variable in the record
        n_distinct_max <- record_variation %>% pivot_longer(col = -c(!!!syms(id_vars)), names_to = "var_name", values_to = "n_distinct") %>%
                group_by(!!!syms(id_vars)) %>% mutate(n_distinct_max = max(n_distinct, na.rm = TRUE)) %>% slice(1) %>% ungroup() %>% 
                select(-c(n_distinct, var_name)) 
        
        
        #######################
        
        
        # join record_count, n_distinct_sum, and n_distinct_max to record_variation
        record_variation <- record_variation %>% left_join(., record_count, by = id_vars) %>% 
                left_join(., n_distinct_sum, by = id_vars) %>% 
                left_join(., n_distinct_max, by = id_vars) %>% 
                rename_at(.vars = vars(-c(!!!syms(id_vars), record_count, n_distinct_sum, n_distinct_max)), .funs = ~ str_c(., "_n_distinct")) %>%
                select(!!!syms(id_vars), record_count, n_distinct_sum, n_distinct_max, everything())
        
        
        #######################
        
        
        # arrange by descending n_distinct_sum, based on row sum
        if(arrange_by == "sum") {
                return(record_variation %>% arrange(desc(n_distinct_sum))) 
        }
        
        # arrange by descending n_distinct_max, based on row max
        if(arrange_by == "max") {
                return(record_variation %>% arrange(desc(n_distinct_max)))
        }
        
        # arrange by descending record_count, based on count
        if(arrange_by == "record_count") {
                return(record_variation %>% arrange(desc(record_count)))
        }
}


#####################
        

# # test
# id_vars <- vars(homeworld)
# 
# starwars %>% filter(species == "Human") %>% count(species, gender, homeworld, mass, height) %>% arrange(desc(n))
# data <- starwars %>% filter(species == "Human") %>% select(species, gender, homeworld, mass, height)
# data
# 
# data %>% get_record_variation(id_vars = "homeworld")
# data %>% get_record_variation(id_vars = homeworld, arrange_by = "max")
# data %>% get_record_variation(id_vars = vars(homeworld), arrange_by = "record_count")
# data %>% get_record_variation(id_vars = vars(homeworld, gender))
# data %>% get_record_variation(id_vars = vars(homeworld, gender), arrange_by = "max")
# data %>% select(homeworld, species, gender, height) %>% get_record_variation(id_vars = vars(homeworld), arrange_by = "record_count")


