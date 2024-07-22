# # load get_t_test()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("get_t_test.R")
# setwd(current_wd)


library(tidyverse)

get_t_test <- function(data, group_var, group_1, group_2, var, conf_level = .95, 
                       alternative = "two.sided", paired = FALSE) {
        
        # handle group_var arg, converting input to strings
        
        # handle single bare variables passed as group_vars
        if(deparse(substitute(group_var)) %in% names(data)) {
                
                group_var <- deparse(substitute(group_var))
                
        } else if("quosure" %in% class(group_var) | "quosures" %in% class(group_var)) {
                
                # handle group_vars if it's passed using quo(), quos(), or group_vars(), including tidyselect helpers
                group_var <- data %>% ungroup() %>% select(!!!(group_var)) %>% names()
                
        } else if(class(group_var) == "character") {
                
                # handle var as a string
                group_var <- group_var
        }
        
        
        #////////////////////////////////////////////////////////////////////////////////////////
        
        
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
        
        
        # get group_1 stats
        group_1_n_valid <- data %>% 
                filter(!!sym(group_var) == group_1) %>%
                filter(!is.na(!!sym(var))) %>% nrow()
        group_1_n_missing <- data %>% 
                filter(!!sym(group_var) == group_1) %>%
                filter(is.na(!!sym(var))) %>% nrow()
        group_1_n_total <- data %>% filter(!!sym(group_var) == group_1) %>% nrow()
        group_1_sum <- data %>% 
                filter(!!sym(group_var) == group_1) %>% 
                summarize(sum = sum(!!sym(var), na.rm = TRUE)) %>% 
                pull(sum)
        
        # get group_2 stats
        group_2_n_valid <- data %>% 
                filter(!!sym(group_var) == group_2) %>%
                filter(!is.na(!!sym(var))) %>% nrow()
        group_2_n_missing <- data %>% 
                filter(!!sym(group_var) == group_2) %>%
                filter(is.na(!!sym(var))) %>% nrow()
        group_2_n_total <- data %>% filter(!!sym(group_var) == group_2) %>% nrow()
        group_2_sum <- data %>% 
                filter(!!sym(group_var) == group_2) %>% 
                summarize(sum = sum(!!sym(var), na.rm = TRUE)) %>% 
                pull(sum)
        
        
        #////////////////////////////////////////////////////////////////////////////////////////
        
        
        # get output w t.test
        output <- t.test(x = data %>% filter(!!sym(group_var) == group_1) %>% select(!!sym(var)), 
               y = data %>% filter(!!sym(group_var) == group_2) %>% select(!!sym(var)),
               conf.level = conf_level, alternative = alternative, paired = paired) %>% 
                tidy() %>%
                mutate(group_var = group_var,
                       group_1 = group_1,
                       group_1_n_total = group_1_n_total,
                       group_1_n_valid = group_1_n_valid,
                       group_1_n_missing = group_1_n_missing,
                       group_1_sum = group_1_sum,
                       group_2 = group_2,
                       group_2_n_total = group_2_n_total,
                       group_2_n_valid = group_2_n_valid,
                       group_2_n_missing = group_2_n_missing,
                       group_2_sum = group_2_sum,
                       stat = "mean",
                       stat_diff = estimate,
                       conf_level = conf_level,
                       alternative = alternative,
                       p_value_significance_threshold = (1 - conf_level),
                       significance_flag = case_when(p.value < p_value_significance_threshold ~ 1, TRUE ~ 0)) %>%
                rename(group_1_stat_value = estimate1,
                       group_2_stat_value = estimate2,
                       test_statistic = statistic,
                       p_value = p.value,
                       conf_int_lower = conf.low,
                       conf_int_upper = conf.high) %>%
                select(group_var, 
                       group_1, group_1_n_total, group_1_n_valid, group_1_n_missing, group_1_sum,
                       group_2, group_2_n_total, group_2_n_valid, group_2_n_missing, group_2_sum,
                       stat, group_1_stat_value, group_2_stat_value, stat_diff,
                       conf_int_lower, conf_int_upper,
                       significance_flag, conf_level, p_value, p_value_significance_threshold, test_statistic, alternative)
        
        
        #////////////////////////////////////////////////////////////////////////////////////////
        
        
        # return output
        return(output)
}


#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////


# # test
# 
# data <- starwars
# group_var <- "sex"
# group_1 <- "male" 
# group_2 <- "female"
# var <- "height"
# alternative <- "two.sided"
# conf_level <- .95
# paired <- FALSE
# 
# t.test(x = data %>% filter(!!sym(group_var) == group_1) %>% select(!!sym(var)), 
#        y = data %>% filter(!!sym(group_var) == group_2) %>% select(!!sym(var)),
#        conf.level = conf_level, alternative = alternative, paired = paired) %>% 
#         tidy()
# 
# 
# #///////////////////////////////////////////////////////////////////////////////////////////
# 
# 
# # inspect
# starwars %>%
#         group_by(sex) %>%
#         get_conf_int(var = "height", stat = "mean") %>%
#         arrange(stat_value, sex)
# 
# starwars %>% get_t_test(group_var = sex, group_1 = "male", group_2 = "female", var = height, 
#                         conf_level = .95, alternative = "two.sided", paired = FALSE) %>%
#         glimpse()
# starwars %>% get_t_test(group_var = vars(sex), group_1 = "male", group_2 = "female", var = "height",
#                         conf_level = .90) %>%
#         glimpse()
# starwars %>% get_t_test(group_var = sex, group_1 = "male", group_2 = "female", var = height) %>% skim()
