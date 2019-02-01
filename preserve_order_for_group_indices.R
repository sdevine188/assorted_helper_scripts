# https://community.rstudio.com/t/why-does-group-indices-use-alphabetical-ordering/5452/4

# create data
df <- tibble(category = c("c", "b", "a", "c"), value = c(7, 1, 4, 2))
df

# the default behavior for group_indices is to order groups alphabetically
df %>% mutate(id = group_indices(., category))

# create preserve_order_for_group_indices function
preserve_order_for_group_indices <- function(x) {
        match(x, unique(x))
}

# test match()
# match returns a vector of the positions of (first) matches of its first argument in its second.
unique(df$category)
match(df$category, unique(df$category))

# preserve_order_for_group_indices retains the original ordering of groups in the data
df %>% mutate(id = group_indices(., category) %>% preserve_order_for_group_indices())
