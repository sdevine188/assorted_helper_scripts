library(tidyverse)

# create add_dummies function
add_dummies <- function(data, vars, drop_vars = FALSE) {
        
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
        
        
        # create get_dummies function
        get_dummies <- function(data, current_var, current_value) {

                # get current_value_clean when current_value is NA
                if(is.na(current_value)) {
                        current_value_clean <- "NA"
                }
                
                # get current_value_clean when current_value is not NA
                if(!(is.na(current_value))) {
                        current_value_clean <- current_value
                }
                
                # get current_dummy_name based on current_var and current_value
                current_dummy_name <- str_c(current_var, ".", current_value_clean)
          
                # create current_var_value_dummy_tbl if current_value is NA
                if(is.na(current_value)) {
                        current_var_value_dummy_tbl <- data %>%
                            mutate(!!sym(current_dummy_name) := case_when(is.na(!!sym(current_var)) ~ 1,
                                        TRUE ~ 0)) %>% select(!!sym(current_dummy_name))
                }
                
                # create current_var_value_dummy_tbl if current_value is not NA
                if(!(is.na(current_value))) {
                        current_var_value_dummy_tbl <- data %>%
                            mutate(!!sym(current_dummy_name) := case_when(!!sym(current_var) == 
                                current_value_clean ~ 1, TRUE ~ 0)) %>% select(!!sym(current_dummy_name))
                }
                
                # return current_var_value_dummy_tbl
                return(current_var_value_dummy_tbl)
        }
        
        
        ###################
        
        
        # create loop_through_vars function
        loop_through_values <- function(data, current_var) {
                
                # get current_var_values
                current_var_values <- data %>% distinct(!!sym(current_var)) %>% pull(!!sym(current_var))
                
                # loop through values calling get_dummies() to get current_var_dummy_tbl
                current_var_dummy_tbl <- map_dfc(.x = current_var_values, 
                    .f = ~ get_dummies(data = data, current_var = current_var, current_value = .x))

                # return current_var_dummy_tbl
                return(current_var_dummy_tbl)
        }
        
        
        ########################
        
        
        # loop through vars calling loop_through_values() to get vars_dummy_tbl
        vars_dummy_tbl <- map_dfc(.x = var_names, .f = ~ loop_through_values(data = data, current_var = .x))
        
        
        #####################
        
        
        # if drop_vars = FALSE, bind vars_dummy_tbl with data and return
        if(drop_vars == FALSE) {
                return(bind_cols(data, vars_dummy_tbl))
        }
        
        # if drop_vars = TRUE, bind vars_dummy_tbl with data, drop vars, and return
        if(drop_vars == TRUE) {
                return(bind_cols(data, vars_dummy_tbl) %>% select(-c(!!!syms(var_names))))
        }
        
        
}


############################


# # test add_dummies()
# starwars %>% add_dummies(vars = "gender") %>% glimpse()
# starwars %>% add_dummies(vars = "gender") %>% select(starts_with("gender"))
# starwars %>% add_dummies(vars = gender) %>% select(starts_with("gender"))
# starwars %>% add_dummies(vars = vars(gender)) %>% select(starts_with("gender"))
# starwars %>% add_dummies(vars = vars(gender, species)) %>% select(starts_with("gender"), starts_with("species"))
# starwars %>% add_dummies(vars = vars(gender, species), drop_vars = TRUE) %>%
#         select(starts_with("gender"), starts_with("species"))


