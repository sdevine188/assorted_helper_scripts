# # load add_numeric_bin()
# current_wd <- getwd()
# setwd("C:/Users/Stephen/Desktop/R/assorted_helper_scripts")
# source("add_numeric_bin.R")
# setwd(current_wd)

library(tidyverse)
library(rlang)

options(scipen = 999)


#///////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////


# create add_numeric_bin()

add_numeric_bin <- function(data, vars, bin_type = c("interval", "number", "width"), bin_n) {
        
        # handle vars passed as single bare variable, character vector, or quosures
        if(deparse(substitute(vars)) %in% names(data)) {
                
                vars <- deparse(substitute(vars))
                
        } else if("quosure" %in% class(vars) | "quosures" %in% class(vars)) {
                
                # handle vars if it's passed using quo(), quos(), or vars(), including tidyselect helpers
                vars <- data %>% ungroup() %>% select(!!!(vars)) %>% names()
                
        } else if(class(vars) == "character") {
                
                # handle vars as a string
                vars <- vars
        }
        
        
        #///////////////////////////////////////////////////////////////////////////////////////////////////
        #///////////////////////////////////////////////////////////////////////////////////////////////////
        #///////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # create get_cut_width_bin()
        get_cut_width_bin <- function(data, var, bin_n) {
                
                # get current_var
                current_var <- var
                
                # get output_var_name
                output_var_name <- str_c(current_var, "_cut_", bin_type, "_", bin_n, "_bin")
                
                
                #///////////////////////////////////
                
                
                # get output
                output <- data %>% 
                        mutate(numeric_bin_output_var = as.character(cut_width(x = !!sym(current_var), width = bin_n)),
                               numeric_bin_output_var = str_replace_all(string = numeric_bin_output_var, 
                                                                        pattern = regex("\\(|\\[|\\)|\\]"), replacement = ""),
                               numeric_bin_output_var_range = str_replace_all(string = numeric_bin_output_var, 
                                                                              pattern = regex(","), replacement = "_to_"),
                               numeric_bin_output_var_start = str_extract(string = numeric_bin_output_var, 
                                                                          pattern = regex("^[0-9]*\\.?[0-9]*\\,")),
                               numeric_bin_output_var_start = as.numeric(str_replace_all(string = numeric_bin_output_var_start, pattern = regex("\\,"), 
                                                                                         replacement = "")),
                               numeric_bin_output_var_end = str_extract(string = numeric_bin_output_var, 
                                                                        pattern = regex("\\,[0-9]*\\.?[0-9]*$")),
                               numeric_bin_output_var_end = as.numeric(str_replace_all(string = numeric_bin_output_var_end, pattern = regex("\\,"), 
                                                                                       replacement = "")))
                
                output <- output %>% distinct(numeric_bin_output_var_start, numeric_bin_output_var_end) %>% 
                        arrange(numeric_bin_output_var_start) %>%
                        mutate(numeric_bin_output_var_bin_number = row_number()) %>%
                        select(numeric_bin_output_var_start, numeric_bin_output_var_bin_number) %>%
                        left_join(output, ., by = "numeric_bin_output_var_start") %>%
                        mutate(!!sym(output_var_name) := str_c("bin_", numeric_bin_output_var_bin_number, "_from_", 
                                                               numeric_bin_output_var_range)) %>%
                        select(!!sym(output_var_name))
                
                
                #/////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # return output
                return(output)
        }
        
        
        #///////////////////////////////////////////////////////////////////////////////////////////////////
        #///////////////////////////////////////////////////////////////////////////////////////////////////
        #///////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # create get_cut_interval_bin()
        get_cut_interval_bin <- function(data, var, bin_n) {
                
                # get current_var
                current_var <- var
                
                # get output_var_name
                output_var_name <- str_c(current_var, "_cut_", bin_type, "_", bin_n, "_bin")
                
                
                #///////////////////////////////////
                
                
                # get output
                output <- data %>% 
                        mutate(numeric_bin_output_var = as.character(cut_interval(x = !!sym(current_var), n = bin_n)),
                               numeric_bin_output_var = str_replace_all(string = numeric_bin_output_var, 
                                                                        pattern = regex("\\(|\\[|\\)|\\]"), replacement = ""),
                               numeric_bin_output_var_range = str_replace_all(string = numeric_bin_output_var, 
                                                                              pattern = regex(","), replacement = "_to_"),
                               numeric_bin_output_var_start = str_extract(string = numeric_bin_output_var, 
                                                                          pattern = regex("^[0-9]*\\.?[0-9]*\\,")),
                               numeric_bin_output_var_start = as.numeric(str_replace_all(string = numeric_bin_output_var_start, pattern = regex("\\,"), 
                                                                                         replacement = "")),
                               numeric_bin_output_var_end = str_extract(string = numeric_bin_output_var, 
                                                                        pattern = regex("\\,[0-9]*\\.?[0-9]*$")),
                               numeric_bin_output_var_end = as.numeric(str_replace_all(string = numeric_bin_output_var_end, pattern = regex("\\,"), 
                                                                                       replacement = "")))
                
                output <- output %>% distinct(numeric_bin_output_var_start, numeric_bin_output_var_end) %>% 
                        arrange(numeric_bin_output_var_start) %>%
                        mutate(numeric_bin_output_var_bin_number = row_number()) %>%
                        select(numeric_bin_output_var_start, numeric_bin_output_var_bin_number) %>%
                        left_join(output, ., by = "numeric_bin_output_var_start") %>%
                        mutate(!!sym(output_var_name) := str_c("bin_", numeric_bin_output_var_bin_number, "_from_", 
                                                               numeric_bin_output_var_range)) %>%
                        select(!!sym(output_var_name))
                
                
                #/////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # return output
                return(output)
        }
        
        
        #///////////////////////////////////////////////////////////////////////////////////////////////////
        #///////////////////////////////////////////////////////////////////////////////////////////////////
        #///////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        # create get_cut_number_bin()
        get_cut_number_bin <- function(data, var, bin_n) {
                
                # get current_var
                current_var <- var
                
                # get output_var_name
                output_var_name <- str_c(current_var, "_cut_", bin_type, "_", bin_n, "_bin")
                
                
                #///////////////////////////////////
                
                
                # get output
                output <- data %>% 
                        mutate(numeric_bin_output_var = as.character(cut_number(x = !!sym(current_var), n = bin_n)),
                               numeric_bin_output_var = str_replace_all(string = numeric_bin_output_var, 
                                                                        pattern = regex("\\(|\\[|\\)|\\]"), replacement = ""),
                               numeric_bin_output_var_range = str_replace_all(string = numeric_bin_output_var, 
                                                                              pattern = regex(","), replacement = "_to_"),
                               numeric_bin_output_var_start = str_extract(string = numeric_bin_output_var, 
                                                                          pattern = regex("^[0-9]*\\.?[0-9]*\\,")),
                               numeric_bin_output_var_start = as.numeric(str_replace_all(string = numeric_bin_output_var_start, pattern = regex("\\,"), 
                                                                                         replacement = "")),
                               numeric_bin_output_var_end = str_extract(string = numeric_bin_output_var, 
                                                                        pattern = regex("\\,[0-9]*\\.?[0-9]*$")),
                               numeric_bin_output_var_end = as.numeric(str_replace_all(string = numeric_bin_output_var_end, pattern = regex("\\,"), 
                                                                                       replacement = "")))
                
                output <- output %>% distinct(numeric_bin_output_var_start, numeric_bin_output_var_end) %>% 
                        arrange(numeric_bin_output_var_start) %>%
                        mutate(numeric_bin_output_var_bin_number = row_number()) %>%
                        select(numeric_bin_output_var_start, numeric_bin_output_var_bin_number) %>%
                        left_join(output, ., by = "numeric_bin_output_var_start") %>%
                        mutate(!!sym(output_var_name) := str_c("bin_", numeric_bin_output_var_bin_number, "_from_", 
                                                               numeric_bin_output_var_range)) %>%
                        select(!!sym(output_var_name))
                
                
                #/////////////////////////////////////////////////////////////////////////////////////////////////////
                
                
                # return output
                return(output)
        }
        
        
        #///////////////////////////////////////////////////////////////////////////////////////////////////
        #///////////////////////////////////////////////////////////////////////////////////////////////////
        #///////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        if(bin_type == "width"){
                
                output <- map(.x = vars, .f = ~ data %>% get_cut_width_bin(var = .x, bin_n = bin_n)) %>%
                        bind_cols() %>%
                        bind_cols(data, .)
                
                return(output)
                
        }
        
        
        #///////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        if(bin_type == "interval"){
                
                output <- map(.x = vars, .f = ~ data %>% get_cut_interval_bin(var = .x, bin_n = bin_n)) %>%
                        bind_cols() %>%
                        bind_cols(data, .)
                
                return(output)
                
        }
        
        
        #///////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        if(bin_type == "number"){
                
                output <- map(.x = vars, .f = ~ data %>% get_cut_number_bin(var = .x, bin_n = bin_n)) %>%
                        bind_cols() %>%
                        bind_cols(data, .)
                
                return(output)
                
        }
}


#///////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////


# # test
# 
# data <- starwars %>% select(name, mass, height, birth_year) %>% filter(mass < 500)
# data
# vars <- c("mass", "height", "birth_year")
# current_var <- "mass"
# bin_type <- "interval"
# bin_n <- 3
# 
# 
# #///////////////////////////////////////////////////////////////////////////////////////////////////
# 
# 
# data %>% add_numeric_bin(vars = vars, bin_type = "interval", bin_n = 3)
# data %>% add_numeric_bin(vars = vars, bin_type = "width", bin_n = 3)
# data %>% add_numeric_bin(vars = vars, bin_type = "number", bin_n = 3)
# 
# data %>% add_numeric_bin(vars = vars(mass, height, birth_year), bin_type = "interval", bin_n = 2)
# data %>% add_numeric_bin(vars = mass, bin_type = "width", bin_n = 10)
# data %>% add_numeric_bin(vars = "height", bin_type = "number", bin_n = 5)


