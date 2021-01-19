library(tidyverse)

# https://www.tidyverse.org/blog/2019/11/scales-1-1-0/
# similar to label_number_si, but more control

# create add_number_label function
add_number_label <- function(breaks, label) {
        str_c(breaks, label)
}


#/////////////////


# test
# starwars %>% count(homeworld, name = "count_of_people_from_homeworld") %>%
#         arrange(desc(count_of_people_from_homeworld)) %>% filter(!is.na(homeworld)) %>% slice(1:5) %>%
#         ggplot(data = ., mapping = aes(x = fct_reorder(.f = homeworld, .x = count_of_people_from_homeworld, .desc = TRUE),
#                                                        y = count_of_people_from_homeworld)) +
#         geom_col() +
#         scale_y_continuous(breaks = seq(from = 1, to = 11, by = 1),
#                 label = add_number_label(breaks = seq(from = 1, to = 11, by = 1), label = " mil people"))
