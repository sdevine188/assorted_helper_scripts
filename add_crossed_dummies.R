# # load add_crossed_dummies()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("add_crossed_dummies.R")
# setwd(current_wd)

library(tidyverse)
library(rlang)

# need to handle apostrophes in variables - see gender/species run
# it chokes on "Yoda's" when trying get_crossed_dummy_vectors, because the parse_expr gets confused with multiple apostrophes

# create add_crossed_dummies function
add_crossed_dummies <- function(data, vars, drop_vars = FALSE, prefix = NULL, suffix = NULL) {
        
        # get var_names from vars
        
        # handle single bare variables passed as vars
        if(deparse(substitute(vars)) %in% names(data)) {
                
                var_names <- deparse(substitute(vars))
                
        } else if("quosure" %in% class(vars) | "quosures" %in% class(vars)) {
                
                # handle vars if it's passed using quo(), quos(), or vars(), including tidyselect helpers
                var_names <- data %>% ungroup() %>% select(!!!vars) %>% names()
                
        } else if(class(vars) == "character") {
                
                # handle vars as a string
                var_names <- vars
        }
        
        
        ##################################################################################
        
        
        # get distinct_var_combos
        distinct_var_combos <- data %>% distinct(!!!syms(var_names))
        
        
        #########################
        
        
        # create get_var_value_combos function
        get_var_value_combos <- function(data, current_var_name, current_var_number) {
                
                # get var_name_output and var_value_output, 
                # which are used to compile the case_when expression, and the dummy output var name
                var_name_output <- str_c("var_", current_var_number, "_name")
                var_value_output <- str_c("var_", current_var_number, "_value")
                var_value_character_output <- str_c("var_", current_var_number, "_value_character")
                var_name_and_value_output <- str_c("var_", current_var_number, "_name_and_value")
                
                # get var_value_combos
                # note that when an apostrophe is in the value, it is substitute internally to "*$*", 
                # this avoids erros when compiling the filter expressions, since apostrophes
                # get confused with single quotes
                # note since the filter expression is built with the substitute, 
                # and is later filtered on a substituted version of the data in get_crossed_dummy_vectors,
                # but then the dummy vector is binded to the original data, the substitute doesn't affect final output values
                return(data %>% select(!!sym(current_var_name)) %>%
                               mutate(!!sym(current_var_name) := str_replace_all(string = !!sym(current_var_name), 
                                                                                 pattern = "'", replacement = "*$*")) %>%
                               mutate(!!sym(var_name_output) := current_var_name) %>%
                               rename(!!sym(var_value_output) := current_var_name) %>%
                               mutate(!!sym(var_value_character_output) := case_when(is.na(!!sym(var_value_output)) ~ "NA",
                                                                                     TRUE ~ as.character(!!sym(var_value_output)))) %>%
                               unite(col = !!sym(var_name_and_value_output), !!sym(var_name_output), 
                                     !!sym(var_value_output), remove = FALSE, sep = "."))
        }
        
        
        ###################
        
        
        # call get_var_value_combos to get crossed_dummy_combos
        crossed_dummy_combos <- map2_dfc(.x = var_names, .y = 1:length(var_names), 
                                         .f = ~ get_var_value_combos(data = data, 
                                                                     current_var_name = .x, current_var_number = .y)) %>%
                unite(col = "crossed_dummy_var_name", tidyselect::matches("var_[1-9]+_name_and_value"), 
                      sep = "_x_", remove = FALSE) %>%
                mutate(crossed_dummy_var_name = str_c(prefix, crossed_dummy_var_name, suffix))
        
        
        ###################
        
        
        # create get_individual_filter_expr function
        get_individual_filter_expr <- function(data, current_var_number) {
                
                # get current_var_filter_expr_sym 
                current_var_filter_expr_sym <- sym(str_c("var_", current_var_number, "_filter_expr"))
                
                # get current_var_name_sym
                current_var_name_sym <- sym(str_c("var_", current_var_number, "_name"))
                
                # get current_var_name_distinct
                current_var_name_distinct <- data %>% distinct(!!current_var_name_sym) %>% pull()
                
                # get current_var_value_sym
                current_var_value_sym <- sym(str_c("var_", current_var_number, "_value"))
                
                # get filter_expr for current_var_number
                # note the mutate/case_when corrects the output when the current_var_value is NA
                return(data %>% 
                               mutate(!!current_var_filter_expr_sym := str_c(!!current_var_name_sym, " == '", 
                                                                             !!current_var_value_sym, "'")) %>%
                               select(crossed_dummy_var_name, !!current_var_filter_expr_sym)) %>%
                        mutate(!!current_var_filter_expr_sym := case_when(is.na(!!current_var_filter_expr_sym) ~
                                                                                  str_c("is.na(", current_var_name_distinct, ")"),
                                                                          TRUE ~ !!current_var_filter_expr_sym))
        }
        
        
        ###################
        
        
        # call get_individual_filter_expr to update crossed_dummy_combos with crossed_dummy_filter_expr
        crossed_dummy_combos <- map(.x = 1:length(var_names), 
                                    .f = ~ get_individual_filter_expr(data = crossed_dummy_combos, 
                                                                      current_var_number = .x)) %>%
                reduce(.f = bind_cols) %>% select(-matches("crossed_dummy_var_name[0-9]+")) %>%
                unite(col = crossed_dummy_filter_expr, contains("filter_expr"), sep = " & ", remove = FALSE) %>%
                select(-crossed_dummy_var_name) %>% bind_cols(crossed_dummy_combos, .) 
        
        
        ###################
        
        
        # create get_crossed_dummy_vectors function
        get_crossed_dummy_vectors <- function(data, current_crossed_dummy_filter_expr, current_crossed_dummy_var_name) {
                
                # note that when an apostrophe is in the value, it is substitute internally to "*$*", 
                # this avoids erros when compiling the filter expressions, since apostrophes
                # get confused with single quotes
                # note since the filter expression is built with the substitute in get_var_value_combos() above, 
                # and then the dummy vector is binded to the original data, the substitute doesn't affect final output values
                return(data %>% 
                               mutate_all(.funs = ~ str_replace_all(string = ., pattern = "'", replacement = "*$*")) %>%
                               # mutate(!!sym(current_var_name) := str_replace_all(string = !!sym(current_var_name), 
                               #                                                   pattern = "'", replacement = "*$*")) %>%
                               mutate(!!sym(current_crossed_dummy_var_name) := case_when(
                                       !!parse_expr(current_crossed_dummy_filter_expr) ~ 1, TRUE ~ 0)) %>%
                               select(!!sym(current_crossed_dummy_var_name)))
        }
        
        
        ###################
        
        
        # call get_crossed_dummy_vectors to get data_w_crossed_dummies
        data_w_crossed_dummies <- map2_dfc(.x = crossed_dummy_combos %>% distinct(crossed_dummy_filter_expr) %>% 
                                                   pull(crossed_dummy_filter_expr), 
                                           .y = crossed_dummy_combos %>% distinct(crossed_dummy_var_name) %>%
                                                   pull(crossed_dummy_var_name),
                                           .f = ~ get_crossed_dummy_vectors(data = data, 
                                                                            current_crossed_dummy_filter_expr = .x,
                                                                            current_crossed_dummy_var_name = .y)) %>%
                bind_cols(data, .)
        
        
        ##################################################################################
        
        
        # handle drop_vars = TRUE
        if(drop_vars == TRUE) {
                data_w_crossed_dummies <- data_w_crossed_dummies %>% select(-c(!!!syms(var_names)))
        }
        
        
        ###################
        
        
        return(data_w_crossed_dummies)
}


############################


# # test add_dummies()
# # note that list columns are removed since add_crossed_dummies throws warning with them
# starwars_data <- starwars %>% select(-c(films, vehicles, starships))
# starwars_data %>% add_crossed_dummies(vars = "gender") %>% glimpse()
# starwars_data %>% add_crossed_dummies(vars = "gender", prefix = "dummy_") %>% select(matches("gender."))
# starwars_data %>% add_crossed_dummies(vars = gender, suffix = "_dummy") %>% select(starts_with("gender"))
# starwars_data %>% add_crossed_dummies(vars = vars(gender), drop_vars = TRUE) %>% select(starts_with("gender"))
# starwars_data %>% filter(gender %in% c("male", "female")) %>%
#         mutate(movie = ifelse(row_number() < 40, "prequel", "sequel")) %>%
#         add_crossed_dummies(vars = vars(gender, movie)) %>%
#         select(gender, movie, matches("gender|movie")) %>%
#         sample_n(10)
# starwars_data %>% filter(gender %in% c("male", "female")) %>%
#         mutate(movie = ifelse(row_number() < 40, "prequel", "sequel")) %>%
#         add_crossed_dummies(vars = vars(gender, movie), drop_vars = TRUE) %>%
#         select(matches("gender|movie")) %>%
#         sample_n(10)

# starwars %>% filter(species %in% c("Human", "Mirialan")) %>% count(gender, hair_color, species) %>% arrange(desc(n))
# starwars %>% filter(species %in% c("Human", "Mirialan")) %>%
#         add_crossed_dummies(vars = vars(gender, hair_color)) %>%
#         add_dummies(vars = species) %>%
#         select(matches("hair_color.black|hair_color.brown$|species.")) %>%
#         data.frame() %>% UpSetR::upset(nsets = 10)

