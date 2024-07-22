# # load get_prop_test()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("get_prop_test.R")
# setwd(current_wd)


library(tidyverse)

get_prop_test <- function(data, group_var, group_1, group_2, var, conf_level = .95, 
                       alternative = "two.sided", yates_correction = TRUE) {
        
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
        group_1_successes <- data %>% filter(!!sym(group_var) == group_1, !!sym(var) == 1) %>% nrow()
        
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
        group_2_successes <- data %>% filter(!!sym(group_var) == group_2, !!sym(var) == 1) %>% nrow()
        
        
        #////////////////////////////////////////////////////////////////////////////////////////
        
        
        # get output w prop.test
        
        # note that prop.test uses wilson score confidence interval for single proportions
        # https://stats.stackexchange.com/questions/183225/confidence-interval-from-rs-prop-test-differs-from-hand-calculation-and-resul
       
        output <- prop.test(x = c(group_1_successes, group_2_successes), 
                         n = c(group_1_n_valid, group_2_n_valid),
                         conf.level = conf_level, alternative = alternative, correct = yates_correction) %>% 
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
                       stat = "prop",
                       stat_diff = estimate1 - estimate2,
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
# data <- starwars %>% mutate(mass_over_80 = case_when(mass > 80 ~ 1, mass <= 80 ~ 0))
# group_var <- "sex"
# group_1 <- "male"
# group_2 <- "female"
# var <- "mass_over_80"
# alternative <- "two.sided"
# conf_level <- .95
# yates_correction <- TRUE
# 
# group_1_n_valid <- data %>% 
#         filter(!!sym(group_var) == group_1) %>%
#         filter(!is.na(!!sym(var))) %>% nrow()
# group_1_successes <- data %>% filter(!!sym(group_var) == group_1, !!sym(var) == 1) %>% nrow()
# 
# group_2_n_valid <- data %>% 
#         filter(!!sym(group_var) == group_2) %>%
#         filter(!is.na(!!sym(var))) %>% nrow()
# group_2_successes <- data %>% filter(!!sym(group_var) == group_2, !!sym(var) == 1) %>% nrow()
# 
# 
# prop.test(x = c(group_1_successes, group_2_successes), 
#                     n = c(group_1_n_valid, group_2_n_valid),
#                     conf.level = conf_level, alternative = alternative, correct = yates_correction) %>%
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
# starwars %>% 
#         mutate(mass_over_80 = case_when(mass > 80 ~ 1, mass <= 80 ~ 0)) %>%
#         get_prop_test(group_var = sex, group_1 = "male", group_2 = "female", var = mass_over_80,
#                         conf_level = .95, alternative = "two.sided", yates_correction = FALSE) %>%
#         glimpse()
# starwars %>% 
#         mutate(mass_over_80 = case_when(mass > 80 ~ 1, mass <= 80 ~ 0)) %>%
#         get_prop_test(group_var = vars(sex), group_1 = "male", group_2 = "female", var = "mass_over_80",
#                         conf_level = .90) %>%
#         glimpse()
# starwars %>% 
#         mutate(mass_over_80 = case_when(mass > 80 ~ 1, mass <= 80 ~ 0)) %>%
#         get_prop_test(group_var = sex, group_1 = "male", group_2 = "female", var = mass_over_80) %>% skim()
