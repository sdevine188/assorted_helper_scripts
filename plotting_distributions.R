library(tidyverse)
library(ggridges)
library(skimr)
library(ggforce)

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

# numeric variable
starwars %>% ggplot(data = ., aes(x = mass)) + stat_ecdf()

# categorical variable
starwars %>% group_by(species) %>% mutate(species_count = n_distinct(row_number())) %>% ungroup() %>% 
        ggplot(data = ., aes(x = fct_reorder(.f = species, .x = species_count, .desc = TRUE))) + 
        stat_ecdf(aes(group = 1))


##########################


# summarizing quantiles
starwars %>% select(mass) %>% skim()
starwars %>% summarize(quantiles = list(enframe(quantile(x = mass, probs = c(0, .25, .5, .75, 1), na.rm = TRUE)))) %>%
        unnest()


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


# violin plot 

# drawn w quantiles
starwars %>% filter(mass < 1000) %>% 
        # note that NA value for gender is placed last using fct_reorder, so change level to be "gender_na" so it reorders correctly
        mutate(gender = ifelse(is.na(gender), "gender_na", gender), gender = factor(gender)) %>% 
        ggplot(data = ., aes(x = gender, y = mass)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

# drawn w points
starwars %>% filter(mass < 1000) %>% 
        # note that NA value for gender is placed last using fct_reorder, so change level to be "gender_na" so it reorders correctly
        mutate(gender = ifelse(is.na(gender), "gender_na", gender), gender = factor(gender)) %>% 
        ggplot(data = ., aes(x = gender, y = mass)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
        geom_violin() + geom_jitter(height = 0, width = 0.1)


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

# ggridges for data that has bounds, like zero lower limit, and so need to trim distribution
# notice how just using geom_density_ridges below shows density curve over negative values not found in data
starwars %>% mutate(good_flag = sample(x = c(0, 1), size = nrow(starwars), replace = TRUE),
                    days_since_eating = c(rep(0, times = 50), 
                                          sample(x = c(0, 1, 2, 3), size = nrow(starwars) - 50, replace = TRUE))) %>%
        ggplot(data = ., aes(x = days_since_eating, y = factor(good_flag))) +
        geom_density_ridges()
# so below is the modification to show trimmed density curve for these cases of boundaries
starwars %>% mutate(good_flag = sample(x = c(0, 1), size = nrow(starwars), replace = TRUE),
        days_since_eating = c(rep(0, times = 50), 
                                          sample(x = c(0, 1, 2, 3), size = nrow(starwars) - 50, replace = TRUE))) %>%
        ggplot(data = ., aes(x = days_since_eating, y = factor(good_flag), height = ..density..)) + 
        geom_density_ridges(stat = "density", trim = TRUE)


#/////////////////////////////////////////////////////////////////


# geom_sina
# https://ggforce.data-imaginist.com/reference/geom_sina.html

# sina plot with log scale, color mapping, and cut intervals on x axis
midwest %>% ggplot(data = ., aes(x = cut_width(area, 0.02), y = popdensity, color = inmetro)) + geom_sina() +
        scale_y_log10()
