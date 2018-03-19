# create df of flags 
list_a <- c(1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0)
list_b <- c(1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1)
list_c <- c(1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0)
df <- data.frame(list_a = list_a, list_b = list_b, list_c = list_c)
df

# use table function
table(df$list_a, df$list_b)
table(df$list_a, df$list_b, df$list_c)

# create cross tab
df %>% group_by(list_a, list_b, list_c) %>% count() %>% 
        gather(key = origin, value = count, c(list_c)) %>% unite(col = origin_count, c(origin, count)) %>%
        spread(key = origin_count, value = n) %>% modify_at(., .at = c("list_c_0", "list_c_1"), .f = ~ifelse(is.na(.x), 0, .x))


