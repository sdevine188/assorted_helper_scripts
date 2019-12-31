# # load get_group_index()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("add_group_index.R")
# setwd(current_wd)


library(tidyverse)
library(attempt)

# create add_group_index function
add_group_index <- function(data, group) {
        
        # handle group argument, depending on whether it's passed as string, bare variable, or vars()
        
        # will pass row to deparse(substitute()) to see if it's a single bare variable
        # if so, will overwrite row with the deparsed string
        # note this handler won't allow passing multiple bare variables in c()
        # if you want multiple bar variables, pass into vars
        if(deparse(substitute(group)) %in% names(data)) {
                
                group <- deparse(substitute(group))
        } 
        
        # handle row if it's passed using quo(), quos(), or vars(), including tidyselect helpers
        # would be nice to issue custom error when tidyselectors have ONLY invalid vars
        # default is to give uninformative error message "Error in -x : invalid argument to unary operator"
        # can use attempt's try_catch, with custom if()/str_replace function to micro-handle different errors
        if("quosure" %in% class(group) | "quosures" %in% class(group)) {
                
                # handle bare variables passed to vars() that are not found in data
                try_catch(expr = group_placeholder <- data %>% ungroup() %>% select(!!!group) %>% names(), .e = function(e) {
                        
                        if(str_detect(string = as.character(e), 
                                      pattern = regex("object .* not found\n"))) {
                                
                                var_not_found <- str_extract(string = as.character(e), 
                                                             pattern = regex('object .* not found\n$')) %>% 
                                        str_replace(string = ., pattern = "object ", replacement = "") %>% 
                                        str_replace(string = ., pattern = " not found\n$", 
                                                    replacement = "")
                                
                                stop(str_glue("The following variable passed to the group ",
                                              "argument is not found in the data: ",
                                              "{var_not_found}"))
                        } 
                })
                
                # handle cases where all tidyselect helpers are not found in data
                # when this is the case, group is set as character(0)
                if(length(data %>% ungroup() %>% select(!!!group) %>% names()) == 0) {
                        stop(str_glue("The following tidyselect helpers ",
                                      "passed to the group argument do not match to any variables found in the data: ",
                                      "{str_c((map(.x = group, .f = as_label) %>% unlist()), collapse = ', ')}"))
                }
                
                # if no errors have been raised, overwrite group with group_placeholder
                group <- group_placeholder
        }
        
        
        ################################################################################################################################
        
        
        # handle ungrouped tbl
        if(is.null(data %>% groups())) {
                
                # get group index based on order that group values appear in data, left_join group index with data, then return it as vector
                return(data %>% distinct(!!!syms(group)) %>% mutate(group_index = row_number()) %>% left_join(data, ., by = group)) 
        }
        
        
        #######################
        
        
        # handle grouped tbl
        if(!(is.null(data %>% groups()))) {
                
                # get grouping_var from grouped tbl
                grouping_var <- data %>% groups() %>% map(.x = ., .f = ~ as_label(.x)) %>% unlist()
                
                # get grouping_var_syms 
                grouping_var_syms <- syms(grouping_var)
               
                return(data %>% select(!!!grouping_var_syms, !!!group) %>% group_split() %>%
                        map_dfr(.x = ., .f = ~ .x %>% distinct(!!!syms(grouping_var_syms), !!!syms(group)) %>% mutate(group_index = row_number())) %>%
                               left_join(data, ., by = c(grouping_var, group)))
        }
        
}


#################


# test add_group_index()
# data <- starwars
# group <- "species"
# group <- vars("species")
# group <- vars(species)
# group <- vars(species, gender)


###############


# starwars %>% add_group_index(group = "species")
# starwars %>% add_group_index(group = species)
# starwars %>% add_group_index(group = vars(species, gender))
# starwars %>% mutate(movie = sample(x = c("old", "new"), size = nrow(.), replace = TRUE)) %>% 
#         group_by(movie) %>% add_group_index(group = species) %>% print(n = 15)
# starwars %>% mutate(movie = sample(x = c("old", "new"), size = nrow(.), replace = TRUE),
#                     good_or_bad = sample(x = c("good", "bad"), size = nrow(.), replace = TRUE)) %>%
#         group_by(movie) %>% add_group_index(group = vars(species, good_or_bad)) %>% print(n = 15)
# starwars %>% mutate(movie = sample(x = c("old", "new"), size = nrow(.), replace = TRUE),
#                     good_or_bad = sample(x = c("good", "bad"), size = nrow(.), replace = TRUE)) %>%
#         group_by(movie, gender) %>% add_group_index(group = vars(species, good_or_bad)) %>% print(n = 15)



