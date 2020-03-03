# # load get_variation_functions()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("get_variation_functions.R")
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
        
        
        # get id_count_w_variation for each variable, counting how many id's have more than one distinct value
        id_count_w_variation <- data %>% group_by(!!!syms(id_vars)) %>% summarize_at(.vars = vars(everything()), .funs = n_distinct) %>%
                ungroup() %>% select(-c(!!!syms(id_vars))) %>% map2_dfr(.x = ., .y = names(.), .f = function(current_var_values = .x, current_var_name = .y) {
                        tibble(values = current_var_values) %>% mutate(variable = current_var_name) %>% 
                                mutate(id_w_variation_flag = ifelse(current_var_values > 1, 1, 0), 
                                       id_count_w_variation = sum(id_w_variation_flag)) %>% 
                                select(variable, id_count_w_variation) %>% slice(1)
                })
        
        
        ############################
        
        
        # combine n_distinct_max and n_distinct_mean, arrange, and return
        if(arrange_by == "max") {
                return(id_count_w_variation %>% left_join(., n_distinct_max, by = "variable") %>% 
                               left_join(., n_distinct_mean, by = "variable") %>% arrange(desc(n_distinct_max)))
        }
        
        if(arrange_by == "mean") {
                return(id_count_w_variation %>% left_join(., n_distinct_max, by = "variable") %>% 
                               left_join(., n_distinct_mean, by = "variable") %>% arrange(desc(n_distinct_mean)))
        }
        
        if(arrange_by == "id_count") {
                return(id_count_w_variation %>% left_join(., n_distinct_max, by = "variable") %>% 
                               left_join(., n_distinct_mean, by = "variable") %>% arrange(desc(id_count_w_variation)))
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


##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################


# create get_change_in_variable_variation function
get_change_in_variable_variation <- function(before_data, after_data, id_vars, arrange_by = "reduced", change_only = TRUE) {
        
        # handle id_vars arg, converting input to strings
        
        # handle single bare variables passed as id_vars
        if(deparse(substitute(id_vars)) %in% names(before_data)) {
                
                id_vars <- deparse(substitute(id_vars))
                
        } else if("quosure" %in% class(id_vars) | "quosures" %in% class(id_vars)) {
                
                # handle id_vars if it's passed using quo(), quos(), or id_vars(), including tidyselect helpers
                id_vars <- before_data %>% ungroup() %>% select(!!!(id_vars)) %>% names()
                
        } else if(class(id_vars) == "character") {
                
                # handle id_vars as a string
                id_vars <- id_vars
        }
        
        
        ################################################################################################################################
        
        
        # get common_variable_names
        common_variable_names <- tibble(var_names = names(before_data)) %>% 
                inner_join(., tibble(var_names = names(after_data)), by = "var_names") %>% pull(var_names)
        
        
        #############
        
        
        # get before_variable_variation, note it is limited to common_variable_names
        before_variable_variation <- before_data %>% select(!!!syms(common_variable_names)) %>% 
                get_variable_variation(id_vars = vars(id_vars))
        
        before_variable_variation_pivoted <- before_variable_variation %>%
                pivot_longer(cols = -variable, names_to = "summary_variables", values_to = "before_values")
        
        
        ##############
        
        # get after_variable_variation, note it is limited to common_variable_names
        after_variable_variation <- after_data %>% select(!!!syms(common_variable_names)) %>% 
                get_variable_variation(id_vars = vars(id_vars))
        
        after_variable_variation_pivoted <- after_variable_variation %>%
                pivot_longer(cols = -variable, names_to = "summary_variables", values_to = "after_values")
        
        
        ##################################################################################################################
        
        
        # get change_in_record_variation, note it is limited to common_variable_names
        change_in_record_variation <- get_change_in_record_variation(before_data = before_data, after_data = after_data, id_vars = id_vars) %>%
                select(-c(id_vars, record_count, n_distinct_sum, n_distinct_max)) %>%
                map2_dfr(.x = ., .y = names(.), .f = ~ tibble(!!sym(.y) := .x) %>% filter(!!sym(.y) < 0) %>% nrow()) %>% 
                pivot_longer(cols = everything(), names_to = "variable", values_to = "id_count_w_reduced_variation") %>%
                mutate(variable = str_sub(string = variable, start = 1, end = -12))
        
        
        ##################################################################################################################
        
        
        # get change_in_variable_variation
        change_in_variable_variation <- before_variable_variation_pivoted %>% 
                left_join(., after_variable_variation_pivoted, by = c("variable", "summary_variables")) %>%
                mutate(change = after_values - before_values) %>% select(variable, summary_variables, change) %>%
                pivot_wider(names_from = summary_variables, values_from = change) %>% 
                left_join(., change_in_record_variation, by = "variable") %>% 
                select(variable, id_count_w_reduced_variation, id_count_w_variation, n_distinct_max, n_distinct_mean)
        
        
        
        ##################################################################################################################
        
        
        # add before/after_record_variation_output if change_only = FALSE
        if(change_only == FALSE) {
                
                # get before/after_variable_variation_output
                before_variable_variation_output <- before_variable_variation %>% 
                        rename_at(.vars = vars(-variable), .funs = ~ str_c("before__", .))
                after_variable_variation_output <- after_variable_variation %>% 
                        rename_at(.vars = vars(-variable), .funs = ~ str_c("after__", .))
                
                # add before/after_record_variation_output
                change_in_variable_variation <- change_in_variable_variation %>% 
                        left_join(., before_variable_variation_output, by = "variable") %>%
                        left_join(., after_variable_variation_output, by = "variable")
        }
        
        
        ##################################################################################################################
        
        
        # handle arrange_by and return
        
        # arrange by descending id_count_w_reduced_variation
        if(arrange_by == "reduced") {
                return(change_in_variable_variation %>% arrange(desc(id_count_w_reduced_variation)))
        }
        
        # arrange by descending n_distinct_max
        if(arrange_by == "max") {
                return(change_in_variable_variation %>% arrange(n_distinct_max))
        }
        
        # arrange by descending n_distinct_mean
        if(arrange_by == "mean") {
                return(change_in_variable_variation %>% arrange(n_distinct_mean))
        }
        
        # arrange by descending id_count
        if(arrange_by == "id_count") {
                return(change_in_variable_variation %>% arrange(id_count_w_variation))
        }
}


####################################################################################################


# # test
# id_vars <- "homeworld"
# 
# before_data <- starwars %>% filter(species %in% c("Human", "Droid", "Gungan")) %>%
#         select(species, gender, homeworld, mass, height) %>%
#         mutate(before_only_var = ifelse(mass > 85, 1, 0), before_var_wo_variation = 1)
# before_data
# 
# after_data <- before_data %>% filter(gender == "male") %>%
#         mutate(after_only_var = ifelse(height > 170, 1, 0), after_var_wo_variation = 0) %>%
#         select(-c(before_only_var, before_var_wo_variation))
# after_data


#######################


# before_data %>% get_variable_variation(id_vars = homeworld)
# after_data %>% get_variable_variation(id_vars = homeworld)
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = homeworld)
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = homeworld, arrange_by = "id_count")
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = homeworld, arrange_by = "mean")
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = homeworld, arrange_by = "max")
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = homeworld, change_only = FALSE) %>%
#         select(variable, contains("id_count_w_variation"))
# 
# 
# before_data %>% get_variable_variation(id_vars = vars(homeworld, species))
# after_data %>% get_variable_variation(id_vars = vars(homeworld, species))
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = vars(homeworld, species))
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = vars(homeworld, gender), arrange_by = "id_count")
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = vars(homeworld, gender), arrange_by = "mean")
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = vars(homeworld, gender), arrange_by = "max")
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = vars(homeworld, species), change_only = FALSE) %>%
#         select(variable, contains("id_count_w_variation"))


##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################


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


##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################


# create get_change_in_record_variation function
get_change_in_record_variation <- function(before_data, after_data, id_vars, arrange_by = "sum", change_only = TRUE) {
        
        # handle id_vars arg, converting input to strings
        
        # handle single bare variables passed as id_vars
        if(deparse(substitute(id_vars)) %in% names(before_data)) {
                
                id_vars <- deparse(substitute(id_vars))
                
        } else if("quosure" %in% class(id_vars) | "quosures" %in% class(id_vars)) {
                
                # handle id_vars if it's passed using quo(), quos(), or id_vars(), including tidyselect helpers
                id_vars <- before_data %>% ungroup() %>% select(!!!(id_vars)) %>% names()
                
        } else if(class(id_vars) == "character") {
                
                # handle id_vars as a string
                id_vars <- id_vars
        }
        
        
        ################################################################################################################################
        
        
        # get common_variable_names
        common_variable_names <- tibble(var_names = names(before_data)) %>% 
                inner_join(., tibble(var_names = names(after_data)), by = "var_names") %>% pull(var_names)
        
        
        #############
        
        
        # get before_record_variation, note it is limited to common_variable_names
        before_record_variation <- before_data %>% select(!!!syms(common_variable_names)) %>% 
                get_record_variation(id_vars = vars(id_vars))
        
        before_record_variation_pivoted <- before_record_variation %>%
                pivot_longer(cols = -id_vars, names_to = "summary_variables", values_to = "before_values")
        
        
        ##############
        
        # get after_record_variation, note it is limited to common_variable_names
        after_record_variation <- after_data %>% select(!!!syms(common_variable_names)) %>% 
                get_record_variation(id_vars = vars(id_vars))
        
        after_record_variation_pivoted <- after_record_variation %>%
                pivot_longer(cols = -id_vars, names_to = "summary_variables", values_to = "after_values")
        
        
        #############
        
        
        # get change_in_record_variation
        change_in_record_variation <- before_record_variation_pivoted %>% 
                left_join(., after_record_variation_pivoted, by = c(id_vars, "summary_variables")) %>%
                mutate(change = after_values - before_values) %>% select(!!!syms(id_vars), summary_variables, change) %>%
                pivot_wider(names_from = summary_variables, values_from = change)
        
        
        ##############
        
        
        # add before/after_record_variation_output if change_only = FALSE
        if(change_only == FALSE) {
                
                # get before/after_record_variation_output
                before_record_variation_output <- before_record_variation %>% rename_at(.vars = vars(-id_vars), .funs = ~ str_c("before__", .))
                after_record_variation_output <- after_record_variation %>% rename_at(.vars = vars(-id_vars), .funs = ~ str_c("after__", .))
                
                # add before/after_record_variation_output
                change_in_record_variation <- change_in_record_variation %>% left_join(., before_record_variation_output, by = id_vars) %>%
                        left_join(., after_record_variation_output, by = id_vars)
        }
        
        
        ##############
        
        
        # handle arrange_by and return
        
        # arrange by descending n_distinct_sum
        if(arrange_by == "sum") {
                return(change_in_record_variation  %>% arrange(n_distinct_sum))
        }
        
        # arrange by descending n_distinct_max
        if(arrange_by == "max") {
                return(change_in_record_variation  %>% arrange(n_distinct_max))
        }
        
        # arrange by descending record_count
        if(arrange_by == "record_count") {
                return(change_in_record_variation  %>% arrange(record_count))
        }
}


###################################################################################################################


# # test
# # id_vars <- "homeworld"
# 
# before_data <- starwars %>% filter(species %in% c("Human", "Droid", "Gungan")) %>%
#         select(species, gender, homeworld, mass, height) %>%
#         mutate(before_only_var = ifelse(mass > 85, 1, 0))
# before_data
# 
# after_data <- before_data %>% filter(gender == "male") %>%
#         mutate(after_only_var = ifelse(height > 170, 1, 0)) %>% select(-before_only_var)
# after_data


##############


# before_data %>% get_record_variation(id_vars = homeworld)
# after_data %>% get_record_variation(id_vars = homeworld)
# get_change_in_record_variation(before_data = before_data, after_data = after_data, id_vars = homeworld)
# get_change_in_record_variation(before_data = before_data, after_data = after_data, id_vars = homeworld, arrange_by = "max")
# get_change_in_record_variation(before_data = before_data, after_data = after_data, id_vars = homeworld, arrange_by = "record_count")
# get_change_in_record_variation(before_data = before_data, after_data = after_data, id_vars = homeworld, change_only = FALSE) %>%
#         select(homeworld, contains("mass_n_distinct"))
# 
# 
# before_data %>% get_record_variation(id_vars = vars(homeworld, species))
# after_data %>% get_record_variation(id_vars = vars(homeworld, species))
# get_change_in_record_variation(before_data = before_data, after_data = after_data, id_vars = vars(homeworld, species))
# get_change_in_record_variation(before_data = before_data, after_data = after_data, id_vars = vars(homeworld, gender), arrange_by = "max")
# get_change_in_record_variation(before_data = before_data, after_data = after_data, id_vars = vars(homeworld, gender), arrange_by = "record_count")
# get_change_in_record_variation(before_data = before_data, after_data = after_data, id_vars = vars(homeworld, species), change_only = FALSE) %>%
#         select(homeworld, species, contains("mass_n_distinct"))



