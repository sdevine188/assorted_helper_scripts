library(tidyverse)
library(ggridges)

options(scipen=999)

# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
# https://ggplot2.tidyverse.org/reference/geom_rug.html
# https://blog.rstudio.com/2016/08/31/forcats-0-1-0/
# https://ggplot2.tidyverse.org/reference/cut_interval.html


# barplot
starwars %>% ggplot(data = ., aes(x = mass)) + geom_bar()
starwars %>% filter(mass < 1000) %>% 
        mutate(mass_bin = cut_number(x = mass, n = 4)) %>% ggplot(data = ., aes(x = mass_bin)) + geom_bar()
starwars %>% filter(mass < 1000) %>% 
        mutate(mass_bin = cut_interval(x = mass, n = 10)) %>% ggplot(data = ., aes(x = mass_bin)) + geom_bar()
starwars %>% filter(mass < 1000) %>% 
        mutate(mass_bin = cut_width(x = mass, width = 100)) %>% ggplot(data = ., aes(x = mass_bin)) + geom_bar()


###################


# histogram
starwars %>% ggplot(data = ., aes(x = mass)) + geom_histogram()


###########################


# empirical cumulative density function
starwars %>% ggplot(data = ., aes(x = mass)) + stat_ecdf()


##########################


# boxplot
map(.x = starwars, .f = ~ sum(is.na(.x)))
starwars %>% ggplot(data = ., aes(y = mass)) + geom_boxplot()
starwars %>% filter(mass < 1000) %>% ggplot(data = ., aes(y = mass)) + geom_boxplot()
starwars %>% filter(mass < 1000) %>% ggplot(data = ., aes(y = mass, x = gender)) + geom_boxplot()
starwars %>% filter(mass < 1000) %>% 
        # note that NA value for gender is placed last using fct_reorder, so change level to be "gender_na" so it reorders correctly
        mutate(gender = ifelse(is.na(gender), "gender_na", gender), gender = factor(gender)) %>% 
        ggplot(data = ., aes(y = mass, x = fct_reorder(.f = gender, .x = mass, .fun = median))) +
        # ggplot(data = ., aes(y = mass, x = fct_reorder(.f = gender, .x = mass, .fun = max))) +
        geom_boxplot()


#####################


# density plot, with single facet
starwars %>% ggplot(data = ., aes(x = mass)) + geom_density()
starwars %>% ggplot(data = ., aes(x = mass)) + geom_density() + geom_rug()

# density plot with multiple facets 
starwars %>% filter(mass < 1000) %>% ggplot(data = ., aes(x = mass)) + geom_density() + facet_wrap(vars(gender))
# (ggridges only works when y argument is provided, won't work if only x argument is provided)
# starwars %>% filter(mass < 1000) %>% ggplot(data = ., aes(x = mass)) + geom_density_ridges() 
starwars %>% filter(mass < 1000) %>% ggplot(data = ., aes(x = mass, y = gender)) + geom_density_ridges() 
starwars %>% filter(mass < 1000) %>% ggplot(data = ., aes(x = mass, y = gender)) + 
        geom_density_ridges(jittered_points = TRUE, position = position_raincloud()) 
starwars %>% filter(mass < 1000) %>% ggplot(data = ., aes(x = mass, y = gender)) + 
        geom_density_ridges(jittered_points = TRUE, point_shape = "|", position = position_points_jitter(width = 0.05, height = 0)) 




