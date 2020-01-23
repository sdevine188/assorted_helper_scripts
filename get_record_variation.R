# # load get_record_variation()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("get_record_variation.R")
# setwd(current_wd)

library(tidyverse)


# create get_record_variation function
get_record_variation <- function(data, id_vars, record_vars = vars(everything()), arrange_by = "max") {
        
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
        
        
        ###########################
        
        
        # handle record_vars arg, converting input to strings
        
        # handle single bare variables passed as record_vars
        if(deparse(substitute(id_vars)) %in% names(data)) {
                
                record_vars <- deparse(substitute(record_vars))
                
        } else if("quosure" %in% class(record_vars) | "quosures" %in% class(record_vars)) {
                
                # handle record_vars if it's passed using quo(), quos(), or record_vars(), including tidyselect helpers
                record_vars <- data %>% select(!!!(record_vars)) %>% names() 
                
        } else if(class(record_vars) == "character") {
                
                # handle record_vars as a string
                record_vars <- record_vars
        }
        
        
        ################################################################################################################################
        
        
        # get record_variation showing n_distinct for each record_var, for each id
        record_variation <- data %>% select(!!!syms(id_vars), !!!syms(record_vars)) %>% 
                group_by(!!!syms(id_vars)) %>% summarize_at(.vars = vars(everything()), .funs = n_distinct) %>%
                ungroup() 
        
        # get record_count for each id
        record_count <- data %>% select(!!!syms(id_vars), !!!syms(record_vars)) %>% count(!!!syms(id_vars)) %>% rename(record_count = n)
        
        # get n_distinct_sum for each id
        n_distinct_sum <- record_variation %>% pivot_longer(col = -c(!!!syms(id_vars)), names_to = "var_name", values_to = "n_distinct") %>%
                group_by(!!!syms(id_vars)) %>% mutate(n_distinct_sum = sum(n_distinct, na.rm = TRUE)) %>% slice(1) %>% ungroup() %>% 
                select(-c(n_distinct, var_name)) 
        
        # get n_distinct_max for each id
        n_distinct_max <- record_variation %>% pivot_longer(col = -c(!!!syms(id_vars)), names_to = "var_name", values_to = "n_distinct") %>%
                group_by(!!!syms(id_vars)) %>% mutate(n_distinct_max = max(n_distinct, na.rm = TRUE)) %>% slice(1) %>% ungroup() %>% 
                select(-c(n_distinct, var_name)) 
        
        # join record_count, n_distinct_sum, and n_distinct_max to record_variation
        record_variation <- record_variation %>% left_join(., record_count, by = id_vars) %>% 
                left_join(., n_distinct_sum, by = id_vars) %>% 
                left_join(., n_distinct_max, by = id_vars) %>% 
                rename_at(.vars = vars(!!!syms(record_vars)), .funs = ~ str_c(., "_n_distinct"))
        
        
        #######################
        
        
        # arrange by descending n_distinct_sum, based on row sum
        if(arrange_by == "sum") {
                
                return(record_variation %>% arrange(desc(n_distinct_sum))) 
        }
        
        
        ########################
        
        
        # arrange by descending n_distinct_max, based on row max
        if(arrange_by == "max") {
                
                return(record_variation %>% arrange(desc(n_distinct_max)))
        }
}


#####################
        

# # test
# id_vars <- vars(homeworld)
# record_vars <- vars(species, gender, mass, height)
# 
# starwars %>% filter(species == "Human") %>% count(species, gender, homeworld, mass, height) %>% arrange(desc(n))
# data <- starwars %>% filter(species == "Human") %>% select(species, gender, homeworld, mass, height)
# 
# data %>% get_record_variation(id_vars = "homeworld")
# data %>% get_record_variation(id_vars = homeworld)
# data %>% get_record_variation(id_vars = vars(homeworld))
# data %>% get_record_variation(id_vars = vars(homeworld, gender))
# data %>% get_record_variation(id_vars = vars(homeworld, gender), arrange_by = "sum")
# 
# data %>% get_record_variation(id_vars = vars(homeworld), record_vars = vars(species, gender, height))
# data %>% get_record_variation(id_vars = vars(homeworld, gender), record_vars = vars(species, height))
# data %>% get_record_variation(id_vars = vars(homeworld, gender), record_vars = vars(species, height), arrange_by = "sum")






