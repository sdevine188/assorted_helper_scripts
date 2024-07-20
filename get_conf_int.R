# # load get_conf_int()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("get_conf_int.R")
# setwd(current_wd)


library(tidyverse)
library(infer)


#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////


# create get_conf_int() ####

# tidy wrapper for get_confidence_interval of mean or prop

?get_confidence_interval
# distribution = "t": point_estimate should be the output of calculate() with stat = "mean" or stat = "diff in means"
# distribution = "z": point_estimate should be the output of calculate() with stat = "prop" or stat = "diff in props"

get_conf_int_internal <- function(data, var, stat, conf_level = .95, na.rm = TRUE) {
        
        # get conf_level_critical_value
        
        # https://rpubs.com/lumumba99/1035036#:~:text=To%20get%20critical%20values%20of,in%20the%20standard%20normal%20distribution.
        
        conf_level_critical_value <- qnorm(p = (1 - conf_level) / 2, lower.tail = FALSE)
        
        
        #///////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # handle errors
        tryCatch(expr = {
                        
                        # handle mean conf_int
                        
                        if(stat == "mean") {
                                
                                # calculate with infer
                                
                                # get sample_mean
                                sample_mean <- data %>%
                                        specify(response = !!sym(var)) %>%
                                        calculate(stat = "mean")
                                
                                # get sampling_dist
                                sampling_dist <- data %>%
                                        specify(response = !!sym(var)) %>%
                                        assume("t")
                                
                                # get output
                                output <- get_confidence_interval(x = sampling_dist, 
                                                                  level = conf_level, 
                                                                  point_estimate = sample_mean) %>%
                                        rename(conf_int_lower = lower_ci, conf_int_upper = upper_ci) %>%
                                        mutate(var = var,
                                               stat = "mean",
                                               stat_value = sample_mean %>% pull(stat),
                                               conf_level = conf_level) %>%
                                        relocate(conf_int_lower, conf_int_upper, .after = stat_value)
                                
                                
                                #////////////////////
                                
                                
                                # calculate manually 
                                
                                # https://online.stat.psu.edu/stat500/lesson/5/5.4/5.4.1
                                # https://online.stat.psu.edu/stat500/lesson/5/5.3/5.3.1
                                
                                sample_n <- data %>% filter(!is.na(!!sym(var))) %>% nrow()
                                
                                output_manual <- data %>% summarise(sample_sum = sum(!!sym(var), na.rm = na.rm),
                                                                    sample_mean = mean(!!sym(var), na.rm = na.rm),
                                                                    sample_standard_error = sd(!!sym(var), na.rm = na.rm) / sqrt(sample_n)) %>%
                                        mutate(conf_int_lower = sample_mean - (conf_level_critical_value * sample_standard_error),
                                               conf_int_upper = sample_mean + (conf_level_critical_value * sample_standard_error),
                                               conf_level_critical_value = conf_level_critical_value,
                                               margin_of_error = conf_level_critical_value * sample_standard_error,
                                               sample_n = sample_n, 
                                               error = NA_character_) %>%
                                        select(sample_mean, conf_int_lower, conf_int_upper, margin_of_error,
                                               sample_n, sample_sum, 
                                               conf_level_critical_value, sample_standard_error, error) %>%
                                        rename(stat_value_manual = sample_mean, 
                                               conf_int_lower_manual = conf_int_lower,
                                               conf_int_upper_manual = conf_int_upper,
                                               margin_of_error_manual = margin_of_error,
                                               n_manual = sample_n,
                                               sum_manual = sample_sum,
                                               conf_level_critical_value_manual = conf_level_critical_value,
                                               standard_error_manual = sample_standard_error)
                                
                                
                                #//////////////////////////
                                
                                
                                # join output_manual and return output
                                return(output %>% bind_cols(., output_manual))
                        }
                        
                        
                        #/////////////////////////////////////////////////////////////////////////////////////////
                        
                        
                        # handle prop conf_int
                        
                        if(stat == "prop") {
                                
                                
                                # calculate with infer
                                
                                # note that infer requires categorical variable for prop, 
                                # so get_conf_int will require 0 or 1 numeric values, with success = 1, and will convert into character in function
                                
                                # get sample_prop
                                sample_prop <- data %>%
                                        mutate(!!sym(var) := as.character(!!sym(var))) %>%
                                        specify(response = !!sym(var), success = "1") %>%
                                        calculate(stat = "prop")
                                
                                # get sampling_dist
                                sampling_dist <- data %>%
                                        mutate(!!sym(var) := as.character(!!sym(var))) %>%
                                        specify(response = !!sym(var), success = "1") %>%
                                        assume("z")
                                
                                # get output
                                output <- get_confidence_interval(x = sampling_dist, 
                                                                  level = conf_level, 
                                                                  point_estimate = sample_prop) %>%
                                        rename(conf_int_lower = lower_ci, conf_int_upper = upper_ci) %>%
                                        mutate(var = var,
                                               stat = "prop",
                                               stat_value = sample_prop %>% pull(stat),
                                               conf_level = conf_level) %>%
                                        relocate(conf_int_lower, conf_int_upper, .after = stat_value)
                                
                                
                                #///////////////////////////////////////////////////////////////////////////////////
                                
                                
                                # calculate manually
                                
                                # https://online.stat.psu.edu/stat500/lesson/5/5.4/5.4.1
                                # https://online.stat.psu.edu/stat500/lesson/5/5.3/5.3.1
                                
                                sample_n <- data %>% filter(!is.na(!!sym(var))) %>% nrow()
                                
                                output_manual <- data %>% summarise(sample_sum = sum(!!sym(var), na.rm = na.rm),
                                                                    sample_prop = sum(!!sym(var), na.rm = na.rm) / sample_n,
                                                                    sample_standard_error = sqrt( (sample_prop * (1 - sample_prop)) / sample_n )) %>%
                                        mutate(conf_int_lower = sample_prop - (conf_level_critical_value * sample_standard_error),
                                               conf_int_upper = sample_prop + (conf_level_critical_value * sample_standard_error),
                                               conf_level_critical_value = conf_level_critical_value,
                                               margin_of_error = conf_level_critical_value * sample_standard_error,
                                               sample_n = sample_n, 
                                               error = NA_character_) %>%
                                        select(sample_prop, conf_int_lower, conf_int_upper, margin_of_error, sample_n, sample_sum, 
                                               conf_level_critical_value, sample_standard_error, error) %>%
                                        rename(stat_value_manual = sample_prop, 
                                               conf_int_lower_manual = conf_int_lower,
                                               conf_int_upper_manual = conf_int_upper,
                                               margin_of_error_manual = margin_of_error,
                                               n_manual = sample_n,
                                               sum_manual = sample_sum,
                                               conf_level_critical_value_manual = conf_level_critical_value,
                                               standard_error_manual = sample_standard_error)
                                
                                
                                #//////////////////////////
                                
                                
                                # join output_manual and return output
                                return(output %>% bind_cols(., output_manual))
                        }
                },
                
                error = function(current_error) {
                        
                        return(tibble(var = var,
                                      stat = stat,
                                      stat_value = NA_real_,
                                      conf_int_lower = NA_real_,
                                      conf_int_upper = NA_real_,
                                      conf_level = NA_real_,
                                      stat_value_manual = NA_real_,
                                      conf_int_lower_manual = NA_real_,
                                      conf_int_upper_manual = NA_real_,
                                      margin_of_error_manual = NA_real_,
                                      n_manual = NA_real_,
                                      sum_manual = NA_real_,
                                      conf_level_critical_value = NA_real_,
                                      standard_error_manual = NA_real_,
                                      error = as.character(current_error)))                
                        }
        )
} 


#////////////////////////////////////////////////////////////////////////////////////////////////////


# create get_conf_int() ####

get_conf_int <- function(data, var, stat, conf_level = .95, na.rm = TRUE) {
        
        
        # handle var arg, converting input to strings
        
        # handle single bare variables passed as group_vars
        if(deparse(substitute(var)) %in% names(data)) {
                
                var <- deparse(substitute(var))
                
        } else if("quosure" %in% class(var) | "quosures" %in% class(var)) {
                
                # handle group_vars if it's passed using quo(), quos(), or group_vars(), including tidyselect helpers
                var <- data %>% ungroup() %>% select(!!!(var)) %>% names()
                
        } else if(class(var) == "character") {
                
                # handle var as a string
                var <- var
        }
        
        
        #////////////////////////////////////////////////////////////////////////////////////////
        #////////////////////////////////////////////////////////////////////////////////////////
        #////////////////////////////////////////////////////////////////////////////////////////
        
        
        # handle ungrouped tbl
        
        if(data %>% groups() %>% length() == 0) {
                
                output <- data %>% get_conf_int_internal(var = var, stat = stat, 
                                                         conf_level = conf_level, na.rm = na.rm)
                return(output)
        }
        
        
        #////////////////////////////////////////////////////////////////////////////////////////
        
        
        # handle grouped tbl
        
        if(data %>% groups() %>% length() > 0) {
                
                group_by_vars <- data %>% groups() %>% map(.x = ., .f = ~ as_label(.x)) %>% unlist()
                
                output <- map(.x = data %>% 
                            ungroup() %>%
                            add_group_index(group_vars = vars(group_by_vars)) %>%
                            add_count(group_index, name = "group_index_count") %>%
                            distinct(group_index) %>% 
                            pull(group_index),
                    .f = ~ data %>% 
                            ungroup() %>%
                            add_group_index(group_vars = vars(group_by_vars)) %>%
                            filter(group_index == .x) %>%
                            get_conf_int_internal(var = var, stat = stat, 
                                                  conf_level = conf_level, na.rm = na.rm) %>%
                            mutate(group_index = .x) %>%
                            left_join(data %>%
                                              ungroup() %>%
                                              add_group_index(group_vars = vars(group_by_vars)) %>%
                                              add_count(group_index, name = "group_index_count") %>%
                                              distinct(!!!syms(group_by_vars), group_index) %>%
                                              filter(group_index == .x),
                                      ., by = "group_index")) %>%
                        bind_rows()
                
                return(output)
        }
}


#////////////////////////////////////////////////////////////////////////////////////////////////////


# # inspect
# 
# starwars %>% skim(height, mass)
# 
# # note there is a slight negligible difference between infer and manual calculation of mean conf_int
# 
# # check stat = mean w ungrouped tbl
# starwars %>% summarize(height_mean = mean(height, na.rm = TRUE))
# 
# starwars %>% get_conf_int(var = "height", stat = "mean") %>% data.frame()
# starwars %>% get_conf_int(var = height, stat = "mean", conf_level = .95, na.rm = TRUE)
# starwars %>% get_conf_int(var = vars(height), stat = "mean", conf_level = .95, na.rm = TRUE)
# 
# 
# #///////////////////////////
# 
# 
# # note that infer requires at least two observations or it throws "not enough 'x' observations" error
# starwars %>%
#         filter(name == "Jabba Desilijic Tiure") %>%
#         get_conf_int(var = vars(height), stat = "mean", conf_level = .95, na.rm = TRUE) %>%
#         data.frame()
# starwars %>%
#         filter(name == "Jabba Desilijic Tiure") %>%
#         bind_rows(.,
#                   starwars %>%
#                           filter(name == "Ric Olié")) %>%
#         get_conf_int(var = vars(height), stat = "mean", conf_level = .95, na.rm = TRUE)
# 
# 
# # note that infer requires variation in data when calculating mean
# # or it throws "data are essentially constant" error
# starwars %>%
#         filter(name == "Jabba Desilijic Tiure") %>%
#         bind_rows(.,
#                   starwars %>%
#                           filter(name == "Jabba Desilijic Tiure")) %>%
#         get_conf_int(var = vars(height), stat = "mean", conf_level = .95, na.rm = TRUE) %>%
#         data.frame()
# starwars %>%
#         filter(name == "Jabba Desilijic Tiure") %>%
#         bind_rows(.,
#                   starwars %>%
#                           filter(name == "Ric Olié")) %>%
#         get_conf_int(var = vars(height), stat = "mean", conf_level = .95, na.rm = TRUE)
# 
# 
# #////////////////////////////////////////////////////////////////////////////////////////////////////
# 
# 
# # check stat = mean w grouped tbl
# starwars %>% group_by(sex) %>% summarize(height_mean = mean(height, na.rm = TRUE))
# starwars %>% group_by(sex) %>% get_conf_int(var = "height", stat = "mean")
# 
# 
# #////////////////////////////
# 
# 
# starwars %>% group_by(sex, species) %>%
#         summarize(n = n(),
#                   height_mean = mean(height, na.rm = TRUE)) %>%
#         mutate(height_mean = case_when(n <= 1 ~ NA_real_, TRUE ~ height_mean)) %>%
#         arrange(height_mean, sex, species)
# starwars %>% group_by(sex, species) %>%
#         get_conf_int(var = "height", stat = "mean") %>%
#         arrange(stat_value, sex, species)
# 
# 
# #/////////////////////////
# 
# 
# starwars %>%
#         filter(name == "Jabba Desilijic Tiure") %>%
#         bind_rows(.,
#                   starwars %>%
#                           filter(name == "Jabba Desilijic Tiure")) %>%
#         group_by(sex) %>%
#         get_conf_int(var = vars(height), stat = "mean", conf_level = .95, na.rm = TRUE) %>%
#         data.frame()
# 
# starwars %>%
#         filter(name == "Jabba Desilijic Tiure") %>%
#         bind_rows(.,
#                   starwars %>%
#                           filter(name == "Jabba Desilijic Tiure")) %>%
#         group_by(sex, species) %>%
#         get_conf_int(var = vars(height), stat = "mean", conf_level = .95, na.rm = TRUE) %>%
#         data.frame()
# 
# 
# #///////////////////////////////////////////////////////////////////////////////////////////////
# #////////////////////////////////////////////////////////////////////////////////////////////////////
# #////////////////////////////////////////////////////////////////////////////////////////////////////
# 
# 
# # check stat = prop w ungrouped_tbl
# starwars %>%
#         mutate(height_over_180 = case_when(height > 180 ~ 1, TRUE ~ 0)) %>%
#         count(height_over_180) %>%
#         mutate(height_over_180 = as.character(height_over_180)) %>%
#         pivot_wider(names_from = height_over_180, names_glue = "height_over_180_{height_over_180}", values_from = n) %>%
#         mutate(n = height_over_180_0 + height_over_180_1,
#                 height_over_180_prop = height_over_180_1 / (height_over_180_0 + height_over_180_1))
# 
# starwars %>%
#         mutate(height_over_180 = case_when(height > 180 ~ 1, TRUE ~ 0)) %>%
#         get_conf_int(var = height_over_180, stat = "prop") %>%
#         data.frame()
# 
# 
# #///////////////////////////
# 
# 
# # note that infer requires at least two observations or it throws "not enough 'x' observations" error
# starwars %>%
#         mutate(height_over_180 = case_when(height > 180 ~ 1, TRUE ~ 0)) %>%
#         filter(name == "Jabba Desilijic Tiure") %>%
#         get_conf_int(var = vars(height_over_180), stat = "prop", conf_level = .95, na.rm = TRUE) %>%
#         data.frame()
# starwars %>%
#         mutate(height_over_180 = case_when(height > 180 ~ 1, TRUE ~ 0)) %>%
#         filter(name == "Jabba Desilijic Tiure") %>%
#         bind_rows(.,
#                   starwars %>%
#                           mutate(height_over_180 = case_when(height > 180 ~ 1, TRUE ~ 0)) %>%
#                           filter(name == "Ric Olié")) %>%
#         get_conf_int(var = vars(height_over_180), stat = "prop", conf_level = .95, na.rm = TRUE)
# 
# 
# # note that infer does not requires variation in data when calculating prop
# starwars %>%
#         mutate(height_over_180 = case_when(height > 180 ~ 1, TRUE ~ 0)) %>%
#         filter(name == "Jabba Desilijic Tiure") %>%
#         bind_rows(.,
#                   starwars %>%
#                           mutate(height_over_180 = case_when(height > 180 ~ 1, TRUE ~ 0)) %>%
#                           filter(name == "Jabba Desilijic Tiure")) %>%
#         get_conf_int(var = vars(height_over_180), stat = "mean", conf_level = .95, na.rm = TRUE) %>%
#         data.frame()
# 
# 
# #////////////////////////////////////////////////////////////////////////////////////////////////////
# 
# 
# # check stat = prop w grouped_tbl
# starwars %>%
#         mutate(height_over_180 = case_when(height > 180 ~ 1, TRUE ~ 0)) %>%
#         group_by(sex) %>%
#         count(height_over_180) %>%
#         mutate(height_over_180 = as.character(height_over_180)) %>%
#         pivot_wider(names_from = height_over_180, names_glue = "height_over_180_{height_over_180}", values_from = n) %>%
#         mutate(n = height_over_180_0 + height_over_180_1,
#                 height_over_180_prop = height_over_180_1 / (height_over_180_0 + height_over_180_1)) %>%
#         arrange(sex)
# 
# starwars %>%
#         mutate(height_over_180 = case_when(height > 180 ~ 1, TRUE ~ 0)) %>%
#         group_by(sex) %>%
#         get_conf_int(var = height_over_180, stat = "prop") %>%
#         arrange(stat_value, sex) %>%
#         data.frame()
# 
# 
# #/////////////////////
# 
# 
# starwars %>%
#         mutate(height_over_180 = case_when(height > 180 ~ 1, TRUE ~ 0)) %>%
#         group_by(sex, species) %>%
#         count(height_over_180) %>%
#         mutate(height_over_180 = as.character(height_over_180)) %>%
#         pivot_wider(names_from = height_over_180, names_glue = "height_over_180_{height_over_180}", values_from = n) %>%
#         mutate(n = height_over_180_0 + height_over_180_1,
#                height_over_180_prop = height_over_180_1 / (height_over_180_0 + height_over_180_1)) %>%
#         arrange(height_over_180_prop, sex, species)
# 
# starwars %>%
#         mutate(height_over_180 = case_when(height > 180 ~ 1, TRUE ~ 0)) %>%
#         group_by(sex, species) %>%
#         get_conf_int(var = height_over_180, stat = "prop") %>%
#         arrange(stat_value, sex, species)
