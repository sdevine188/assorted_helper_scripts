# # load get_invalid_anumbers function
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("coalesce_vars.R")
# setwd(current_wd)

library(tidyverse)


# create coalesce_vars function
coalesce_vars <- function(tbl, vars, drop_vars = FALSE) {
        
        # get var_names from vars
        
        # handle single bare variables passed as vars
        if(deparse(substitute(vars)) %in% names(data)) {
                
                var_names <- deparse(substitute(vars))
                
        } else if("quosure" %in% class(vars) | "quosures" %in% class(vars)) {
                
                # handle vars if it's passed using quo(), quos(), or vars(), including tidyselect helpers
                var_names <- data %>% select(!!!vars) %>% names()
                
        } else if(class(vars) == "character") {
                
                # handle vars as a string
                var_names <- vars
        }
        
        
        ################################################################################################################################
        
        
        # create var_list, since coalesce works with lists or vectors, not tbls
        var_list <- tbl %>% select(!!!syms(var_names)) %>% map(.x = ., .f = ~ .x)
        
        # splice var_list into dots with !!! and coalesce them
        coalesced_var <- coalesce(!!!var_list)
        
        # add coalesced_var to tbl
        coalesced_var_name <- bind_rows(tibble(var_name = c(var_names, "coalesce"))) %>% pull(var_name) %>% str_c(string = ., collapse = "_") %>% sym()
        tbl <- tbl %>% mutate(!!coalesced_var_name := coalesced_var)
        
        # if drop_vars = TRUE then drop coalesced vars
        if(drop_vars == TRUE) {
                tbl <- tbl %>% select(-c(!!!syms(var_names)))
        }
        
        # return tbl
        return(tbl)
}



########


# test
# starwars %>% coalesce_vars(vars = c("hair_color", "eye_color"))
# starwars %>% coalesce_vars(vars = vars(hair_color, eye_color))
# starwars %>% coalesce_vars(vars = vars(hair_color, eye_color), drop_vars = TRUE)
# starwars %>% mutate(coalesce_hair_eye = coalesce(hair_color, eye_color))

