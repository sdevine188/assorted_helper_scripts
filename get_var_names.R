library(tidyverse)
library(attempt)

# create get_var_names function
get_var_names <- function(var_input) {
        
        # get var names from var_input to function, handling strings, bare variables, and tidy selectors
        
        # handle single bare variables passed as var_input
        # the first negated str_detect condition will return TRUE if var_input is not a character
        # the second negated str_detect condition returns TRUE if var_input deparsed isn't wrapped in "vars()"
        if((!(str_detect(string = deparse(substitute(var_input)), pattern = regex("^\".*\"$|^c\\(\".*\"\\)$")))) &
           (!(str_detect(string = deparse(substitute(var_input)), pattern = regex("^vars\\(.*\\)$"))))) {
                
                return(deparse(substitute(var_input)))
        } else
        
        # handle var_input if it's passed using quo(), quos(), or vars(), including tidyselect helpers
        if((!(str_detect(string = deparse(substitute(var_input)), pattern = regex("^\".*\"$|^c\\(\".*\"\\)$")))) &
            (str_detect(string = deparse(substitute(var_input)), pattern = regex("^vars\\(.*\\)$")))) {
                
                return(var_input %>% map(.x = ., .f = as_label) %>% unlist())
        } else
        
        # handle var_input as a string
        if(class(var_input) == "character") {
                
                return(var_input)
        }
}


##########################


# test get_var_names()
get_var_names(var_input = "species")
get_var_names(var_input = c("species", "homeworld"))
get_var_names(var_input = species)
get_var_names(var_input = vars(species, homeworld))


################


# test get_var_names as a function, used inside another function
# note get_var_names() does not work, because quosures are dependent on environment they're called in
add_one_w_fx <- function(data, var_input) {

        print(var_input)

        # get_var_names
        var_names <- get_var_names(var_input = var_input)
        print(var_names)

        # add one
        return(data %>% select(!!!syms(var_names)) %>% map_dfc(.x = ., .f = ~ .x + 1))
}
starwars %>% add_one_w_fx(vars(mass, height))
starwars %>% select(mass, height)


##############


# test get_var_names as raw code used inside another function
# this works, because raw code is running in same environment as rest of the function
add_one_w_raw_code <- function(data, var_input) {

        # handle single bare variables passed as var_input
        # the first negated str_detect condition will return TRUE if var_input is not a character
        # the second negated str_detect condition returns TRUE if var_input deparsed isn't wrapped in "vars()"
        if((!(str_detect(string = deparse(substitute(var_input)), pattern = regex("^\".*\"$|^c\\(\".*\"\\)$")))) &
           (!(str_detect(string = deparse(substitute(var_input)), pattern = regex("^vars\\(.*\\)$"))))) {

                var_names <- deparse(substitute(var_input))
        } else

        # handle var_input if it's passed using quo(), quos(), or vars(), including tidyselect helpers
        if((!(str_detect(string = deparse(substitute(var_input)), pattern = regex("^\".*\"$|^c\\(\".*\"\\)$")))) &
           (str_detect(string = deparse(substitute(var_input)), pattern = regex("^vars\\(.*\\)$")))) {

                var_names <- var_input %>% map(.x = ., .f = as_label) %>% unlist()
        } else

        # handle var_input as a string
        if(class(var_input) == "character") {

                var_names <- var_input
        }

        # add one
        return(data %>% select(!!!syms(var_names)) %>% map_dfc(.x = ., .f = ~ .x + 1))
}
starwars %>% add_one_w_raw_code(vars(mass, height))
starwars %>% select(mass, height)






#################


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

