# # load get_variable_variation()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("get_variable_variation.R")
# setwd(current_wd)

library(tidyverse)

# create get_variable_variation function
get_variable_variation <- function(data, id_vars, arrange_by = "id_count") {
        
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
        
        
        # get n_distinct_max for each variable, providing summary measure of the max id-level count of n_distinct values for that variable
        n_distinct_max <- data %>% group_by(!!!syms(id_vars)) %>% summarize_at(.vars = vars(everything()), .funs = n_distinct) %>%
                ungroup() %>% select(-c(!!!syms(id_vars))) %>% summarize_at(.vars = vars(everything()), .funs = max) %>%
                pivot_longer(cols = everything(), names_to = "variable", values_to = "n_distinct_max")
        
        
        #############################
        
        
        # get n_distinct_avg for each variable, providing summary measure of the average id-level count of n_distinct values for that variable
        n_distinct_mean <- data %>% group_by(!!!syms(id_vars)) %>% summarize_at(.vars = vars(everything()), .funs = n_distinct) %>%
                ungroup() %>% select(-c(!!!syms(id_vars))) %>% summarize_at(.vars = vars(everything()), .funs = mean) %>%
                pivot_longer(cols = everything(), names_to = "variable", values_to = "n_distinct_mean")
        
        
        ############################
        
        
        # get id_w_variation_count for each variable, counting how many id's have more than one distinct value
        id_w_variation_count <- data %>% group_by(!!!syms(id_vars)) %>% summarize_at(.vars = vars(everything()), .funs = n_distinct) %>%
                ungroup() %>% select(-c(!!!syms(id_vars))) %>% map2_dfr(.x = ., .y = names(.), .f = function(current_var_values = .x, current_var_name = .y) {
                        tibble(values = current_var_values) %>% mutate(variable = current_var_name) %>% 
                                mutate(id_w_variation_flag = ifelse(current_var_values > 1, 1, 0), 
                                       id_w_variation_count = sum(id_w_variation_flag)) %>% 
                                select(variable, id_w_variation_count) %>% slice(1)
                })
        
        
        ############################
        
        
        # combine n_distinct_max and n_distinct_mean, arrange, and return
        if(arrange_by == "max") {
                return(id_w_variation_count %>% left_join(., n_distinct_max, by = "variable") %>% 
                               left_join(., n_distinct_mean, by = "variable") %>% arrange(desc(n_distinct_max)))
        }
        
        if(arrange_by == "mean") {
                return(id_w_variation_count %>% left_join(., n_distinct_max, by = "variable") %>% 
                               left_join(., n_distinct_mean, by = "variable") %>% arrange(desc(n_distinct_mean)))
        }
        
        if(arrange_by == "id_count") {
                return(id_w_variation_count %>% left_join(., n_distinct_max, by = "variable") %>% 
                               left_join(., n_distinct_mean, by = "variable") %>% arrange(desc(id_w_variation_count)))
        }
}


####################################


# # test
# id_vars <- vars(homeworld)
# 
# starwars %>% count(species, gender, homeworld, mass, height) %>% arrange(desc(n))
# data <- starwars %>% select(species, gender, homeworld, mass, height) %>% mutate(no_variation_var = "test")
# data
#
# data %>% get_variable_variation(id_vars = "homeworld")
# data %>% get_variable_variation(id_vars = homeworld, arrange_by = "max")
# data %>% get_variable_variation(id_vars = vars(homeworld), arrange_by = "mean")
# data %>% get_variable_variation(id_vars = vars(homeworld, gender))
# data %>% get_variable_variation(id_vars = vars(homeworld, gender), arrange_by = "mean")
# data %>% select(homeworld, species, gender, height) %>% get_variable_variation(id_vars = vars(homeworld), arrange_by = "max")





