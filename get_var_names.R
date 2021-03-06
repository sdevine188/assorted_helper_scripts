library(tidyverse)
library(attempt)


# test get_var_names as raw code used inside another function
# this works, because raw code is running in same environment as rest of the function
# actually using a get_var_names() function containing this raw code will not work inside another function
# due to how quosures are dependent on their environment
add_one_w_raw_code <- function(data, vars) {

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
        
        
        # add one
        return(data %>% select(!!!syms(var_names)) %>% map_dfc(.x = ., .f = ~ .x + 1))
}


###############


# test add_one_w_raw_code
starwars %>% add_one_w_raw_code(vars(mass, height))
starwars %>% select(mass, height)


##############################################################################


# create get_var_names function
# get_var_names <- function(var_input) {
#         
#         # get var names from var_input to function, handling strings, bare variables, and tidy selectors
#         
#         # handle single bare variables passed as var_input
#         # the first negated str_detect condition will return TRUE if var_input is not a character
#         # the second negated str_detect condition returns TRUE if var_input deparsed isn't wrapped in "vars()"
#         if((!(str_detect(string = deparse(substitute(var_input)), pattern = regex("^\".*\"$|^c\\(\".*\"\\)$")))) &
#            (!(str_detect(string = deparse(substitute(var_input)), pattern = regex("^vars\\(.*\\)$"))))) {
#                 
#                 return(deparse(substitute(var_input)))
#         } else
#                 
#                 # handle var_input if it's passed using quo(), quos(), or vars(), including tidyselect helpers
#                 if("quosure" %in% class(var_input) | "quosures" %in% class(var_input)) {
#                         
#                         return(var_input %>% map(.x = ., .f = as_label) %>% unlist())
#                 } else
#                         
#                         # handle var_input as a string
#                         if(class(var_input) == "character") {
#                                 
#                                 return(var_input)
#                         }
# }


##########################


# test get_var_names()
# get_var_names(var_input = "species")
# get_var_names(var_input = c("species", "homeworld"))
# get_var_names(var_input = species)
# get_var_names(var_input = vars(species, homeworld))


################


# test get_var_names as a function, used inside another function
# note get_var_names() does not work, because quosures are dependent on environment they're called in
# add_one_w_fx <- function(data, var_input) {
#         
#         print(var_input)
#         
#         # get_var_names
#         var_names <- get_var_names(var_input = var_input)
#         print(var_names)
#         
#         # add one
#         return(data %>% select(!!!syms(var_names)) %>% map_dfc(.x = ., .f = ~ .x + 1))
# }


#####################


# test add_one_w_fx
# starwars %>% add_one_w_fx(vars(mass, height))
# starwars %>% select(mass, height)


##############


# example showing regex to catch quosures variables
# the first condition in the regex returns TRUE if it does not match a deparsed string
# the second condition in the regex returns TRUE 
# if its quosures where deparsed string is encased in "vars(...)", 
# and if regex is negated, it returns TRUE for bare variables

# # single string
# deparse(substitute("species"))
# (!(str_detect(string = deparse(substitute("species")), pattern = regex("^\".*\"$|^c\\(\".*\"\\)$")))) &
#         (!(str_detect(string = deparse(substitute(species)), pattern = regex("^vars\\(.*\\)$"))))
#
# 
# # multiple strings
# deparse(substitute("species"))
# (!(str_detect(string = deparse(substitute(c("species", "homeworld"))), 
#               pattern = regex("^\".*\"$|^c\\(\".*\"\\)$")))) &
#         (!(str_detect(string = deparse(substitute(species)), pattern = regex("^vars\\(.*\\)$"))))
#
# 
# # single bare var
# deparse(substitute(species))
# (!(str_detect(string = deparse(substitute(species)), pattern = regex("^\".*\"$|^c\\(\".*\"\\)$")))) &
#         (!(str_detect(string = deparse(substitute(species)), pattern = regex("^vars\\(.*\\)$"))))
#
# 
# # single quosure
# deparse(substitute(vars(species)))
# (!(str_detect(string = deparse(substitute(vars(species))), pattern = regex("^\".*\"$|^c\\(\".*\"\\)$")))) &
#         (str_detect(string = deparse(substitute(vars(species))), pattern = regex("^vars\\(.*\\)$")))
#
# 
# # multiple quosures
# deparse(substitute(vars(species, mass)))
# (!(str_detect(string = deparse(substitute(vars(species))), pattern = regex("^\".*\"$|^c\\(\".*\"\\)$")))) &
#         (str_detect(string = deparse(substitute(vars(species, mass))), pattern = regex("^vars\\(.*\\)$")))

