# # load get_change_in_variable_variation()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("get_change_in_variable_variation.R")
# setwd(current_wd)

library(tidyverse)

# create get_change_in_variable_variation function
get_change_in_variable_variation <- function(before_data, after_data, id_vars, arrange_by = "max", change_only = TRUE) {
        
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
        
        
        #############
        
        
        # get change_in_variable_variation
        change_in_variable_variation <- before_variable_variation_pivoted %>% 
                left_join(., after_variable_variation_pivoted, by = c("variable", "summary_variables")) %>%
                mutate(change = after_values - before_values) %>% select(variable, summary_variables, change) %>%
                pivot_wider(names_from = summary_variables, values_from = change)
        
        
        ##############
        
        
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
        
        
        ##############
        
        
        # handle arrange_by and return
        
        # arrange by descending n_distinct_max
        if(arrange_by == "max") {
                return(change_in_variable_variation  %>% arrange(n_distinct_max))
        }
        
        # arrange by descending n_distinct_mean
        if(arrange_by == "mean") {
                return(change_in_variable_variation  %>% arrange(n_distinct_mean))
        }
        
        # arrange by descending id_count
        if(arrange_by == "id_count") {
                return(change_in_variable_variation  %>% arrange(id_w_variation_count))
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
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = homeworld, arrange_by = "mean")
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = homeworld, arrange_by = "id_count")
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = homeworld, change_only = FALSE) %>%
#         select(variable, contains("id_w_variation_count"))
# 
# 
# before_data %>% get_variable_variation(id_vars = vars(homeworld, species))
# after_data %>% get_variable_variation(id_vars = vars(homeworld, species))
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = vars(homeworld, species))
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = vars(homeworld, gender), arrange_by = "mean")
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = vars(homeworld, gender), arrange_by = "id_count")
# get_change_in_variable_variation(before_data = before_data, after_data = after_data, id_vars = vars(homeworld, species), change_only = FALSE) %>%
#         select(variable, contains("id_w_variation_count"))


