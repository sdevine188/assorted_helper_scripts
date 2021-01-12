library(tidyverse)
library(jcolors)
library(viridis)

# this WCAG 2.0 guidance specifically says it doesn't officially apply to charts, 
# though charts should still take contrast into account; the guidance is more focused on web design
# https://www.w3.org/TR/UNDERSTANDING-WCAG20/visual-audio-contrast-contrast.html
# "Although this Success Criterion only applies to text, 
# similar issues occur for content presented in charts, graphs, diagrams, and other non-text-based information. 
# Content presented in this manner should also have good contrast to ensure that more users can access the information."

# also note that alternative text can be provided as metadata for tables/charts, to meet requirement for extractable info
# https://www.section508.gov/create/documents
# https://www.section508.gov/create/pdfs

# 508.gov guidance specifically links to WebAIM color contrast checker
# https://www.section508.gov/content/guide-accessible-web-design-development
# https://webaim.org/resources/contrastchecker/

# this .gov guidance has many examples of finely graded color palettes like the Blues, 
# and even links to RColorBrewer as a best practice
# https://xdgov.github.io/data-design-standards/components/colors 
# https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3

# this site has some limited and general guidance for charts
# https://designsystem.digital.gov/components/data-visualizations/

# automatic palette generator based off hex input
# https://learnui.design/tools/data-color-picker.html#palette

# best: w3 color hex picker
# https://www.w3schools.com/colors/colors_picker.asp


#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# create data ####
data <- tibble(group_number = as.character(c(rep(seq(from = 1, to = 7, by = 1), times = 5))), 
               name = starwars %>% slice(1:35) %>% pull(name),
               value_1 = sample(seq(from = .1, to = 1, by = .1), size = 35, replace = TRUE),
               value_2 = sample(seq(from = .1, to = 1, by = .1), size = 35, replace = TRUE),
               measure_1 = sample(seq(from = .1, to = 1, by = .1), size = 35, replace = TRUE),
               measure_2 = sample(seq(from = .1, to = 1, by = .1), size = 35, replace = TRUE),
               measure_3 = sample(seq(from = .1, to = 1, by = .1), size = 35, replace = TRUE),
               measure_4 = sample(seq(from = .1, to = 1, by = .1), size = 35, replace = TRUE))
data
data %>% count(group_number)
data %>% count(group_number, name)
data %>% print(n = nrow(.))


#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# create two color bar chart ####

# final pick is blue_grey
# us web design system advocates blue_grey palette as 508 compliant contrast, 
# https://v1.designsystem.digital.gov/components/colors/
# though my palette takes the blues and greys from R Color Brewer
# and us .gov data visualization standards explicitly recommends R Color Brewer as a best practice for 508 compliant contrast
# https://xdgov.github.io/data-design-standards/components/colors

# inspect colors
brewer.blues(9)
display.brewer.pal(n = 10, name = "Blues")
brewer.blues(10)
brewer.greys(10)
display.brewer.pal(n = 9, name = "Greys")


show_col(viridis_pal()(7))
color_palette <- tibble(hex = viridis_pal()(7))

# create blue_grey color palette
show_col(c("#112e51", "#205493", "#0071bc", "#aeb0b5", "#323a45"))
show_col(c("#08306B", "#08519C", "#4292C6", "#9ECAE1", "#BDBDBD", "#737373", "#484848"))
color_palette <- tibble(hex = c("#08306B", "#08519C", "#4292C6", "#9ECAE1", "#BDBDBD", "#737373", "#484848"))

display_jcolors("pal9")
jcolors("pal9")
display_jcolors("pal7")
jcolors("pal7")
display_jcolors("pal5")
jcolors("pal5")
display_jcolors("pal6")
jcolors("pal6")

show_col(c("#08306B", "#2171B5", "#6BAED6"))
show_col(c("#A4BD32", "#24A99C", "#2E6657", "#265448"))
show_col(c("#08306B", "#2171B5", "#6BAED6", "#78909C", "#99ba78", "#24A99C", "#2E6657"))

# create blue_grey_green custom palette
# color_palette <- tibble(hex = c("#08306B", "#2171B5", "#6BAED6", "#78909C", "#99ba78", "#24A99C", "#2E6657"))
# color_palette
# show_col(color_palette %>% pull(hex))

# blue_grey palette supports 7 colors, plus possible extensions via fill/line type
show_col(color_palette %>% slice(1, 4) %>% pull(hex)) # 2 colors
show_col(color_palette %>% slice(1, 2, 4) %>% pull(hex)) # 3 colors
show_col(color_palette %>% slice(1, 2, 3, 4) %>% pull(hex)) # 4 colors
show_col(color_palette %>% slice(1, 2, 4, 6, 7) %>% pull(hex)) # 5 colors
show_col(color_palette %>% slice(1, 2, 4, 5, 6, 7) %>% pull(hex)) # 6 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7) %>% pull(hex)) # 7 colors

# add color_bin and color
chart_data <- data %>% mutate(color_bin = group_number,
                              color = case_when(color_bin == "1" ~ color_palette %>% slice(1) %>% pull(hex),
                                                color_bin == "2" ~ color_palette %>% slice(2) %>% pull(hex),
                                                color_bin == "3" ~ color_palette %>% slice(3) %>% pull(hex),
                                                color_bin == "4" ~ color_palette %>% slice(4) %>% pull(hex),
                                                color_bin == "5" ~ color_palette %>% slice(5) %>% pull(hex),
                                                color_bin == "6" ~ color_palette %>% slice(6) %>% pull(hex),
                                                color_bin == "7" ~ color_palette %>% slice(7) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////


pandem_bar_chart <- chart_data %>% 
        filter(group_number %in% c("1", "4")) %>%
        ggplot(data = ., aes(x = fct_reorder(.f = factor(name), .x = value_1), 
                             y = value_1, 
                             fill = factor(color_bin))) +        
        geom_col() + 
        scale_fill_manual(values = chart_data_color_list) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(0, 1.1), expand = c(0, 0)) +
        labs(x = NULL, y = "Pandemic Democratic Violations Index", 
             title = NULL,
             caption = NULL, fill = "") +
        # coord_fixed(ratio = 1/.05, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_line(color = "#DDDDDD"),
                panel.grid.major.y = element_blank(),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#595959"),
                axis.line.y.left = element_line(color = "#595959"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", 
                                            margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", 
                                            margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5)
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) + 
        coord_flip()


# inspect
pandem_bar_chart


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# blue_grey_green
color_palette <- tibble(hex = c("#08306B", "#2171B5", "#6BAED6", "#78909C", "#99ba78", "#24A99C", "#2E6657"))
color_palette
show_col(color_palette %>% pull(hex))

# add color_bin and color
chart_data <- data %>% mutate(color_bin = group_number,
                                    color = case_when(color_bin == "1" ~ color_palette %>% slice(1) %>% pull(hex),
                                                      color_bin == "2" ~ color_palette %>% slice(2) %>% pull(hex),
                                                      color_bin == "3" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      color_bin == "4" ~ color_palette %>% slice(4) %>% pull(hex),
                                                      color_bin == "5" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      color_bin == "6" ~ color_palette %>% slice(6) %>% pull(hex),
                                                      color_bin == "7" ~ color_palette %>% slice(7) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#/////////////////////


# create scatterplot ####
pandem_panback_scatterplot <- chart_data %>% 
        ggplot(data = ., aes(x = value_1, y = value_2, label = name,
                             color = factor(color_bin, 
                                            levels = c("1", "2", "3", "4", "5", "6", "7")))) +
        geom_point(size = 6) + 
        geom_text_repel(fontface = "bold", point.padding = .2, size = 4) +
        scale_color_manual(values = chart_data_color_list) +
        # scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.01, 1.01)) +
        scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.01, 1.01)) +
        labs(x = "Pandemic Democratic Violations Index", y = "Pandemic Backsliding Index", 
             title = NULL,
             caption = NULL, color = "") +
        # coord_fixed(ratio = 1/.05, clip = "off") +
        # coord_flip() + 
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 13, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_blank(),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#595959"),
                axis.line.y.left = element_line(color = "#595959"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", 
                                            margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", 
                                            margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 18, face = "bold", hjust = .5, family = "Calibri", color = "#000000", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5)
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 


# inspect
pandem_panback_scatterplot


#/////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////


chart_data <- data %>% 
        select(group_number, name, starts_with("measure")) %>%
        pivot_longer(cols = -c(group_number, name), names_to = "var", values_to = "value")


#/////////////////////


# blue_grey_green
color_palette <- tibble(hex = c("#08306B", "#2171B5", "#6BAED6", "#78909C", "#99ba78", "#24A99C", "#2E6657"))
color_palette
show_col(color_palette %>% pull(hex))

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = group_number,
                              color = case_when(color_bin == "1" ~ color_palette %>% slice(1) %>% pull(hex),
                                                color_bin == "2" ~ color_palette %>% slice(2) %>% pull(hex),
                                                color_bin == "3" ~ color_palette %>% slice(3) %>% pull(hex),
                                                color_bin == "4" ~ color_palette %>% slice(4) %>% pull(hex),
                                                color_bin == "5" ~ color_palette %>% slice(5) %>% pull(hex),
                                                color_bin == "6" ~ color_palette %>% slice(6) %>% pull(hex),
                                                color_bin == "7" ~ color_palette %>% slice(7) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#/////////////////////


# create jitter_plot ####
pandem_type_jitter_plot <- chart_data %>% ggplot(data = ., aes(x = var, 
                                        y = value, color = factor(color_bin, 
                        levels = c("1", "2", "3", "4", "5", "6","7")))) +  
        geom_jitter(width = 0.15, height = .25, size = 6) +         
        scale_size(guide = "none") +
        scale_color_manual(values = chart_data_color_list) +
        scale_x_discrete(expand = c(.1, 0)) +
        scale_y_discrete(expand = c(.1, 0)) +
        # scale_x_continuous(breaks = seq(from = 0, to = .8, by = .1), limits = c(-.05, .85), expand = c(-.05, 0)) +
        # scale_y_continuous(breaks = seq(from = 0, to = .6, by = .1), limits = c(-.05, .65), expand = c(-.05, 0)) +
        labs(x = "Pandemic Democratic Violations Index sub-indicators", y = "Type of violation",
             title = NULL,
             caption = NULL, color = "") +
        # coord_fixed(ratio = 1/.05, clip = "off") +
        # coord_flip() + 
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_line(color = "#DDDDDD"),
                panel.grid.major.y = element_blank(),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", hjust = .5,
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#595959"),
                axis.line.y.left = element_line(color = "#595959"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", 
                                            margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", 
                                            margin = margin(t = 0, r = 10, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5)
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) + 
        guides(colour = guide_legend(override.aes = list(size = 4))) +
        coord_flip()

# inspect
pandem_type_jitter_plot


#/////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////


# blue_grey_green
color_palette <- tibble(hex = c("#08306B", "#2171B5", "#6BAED6", "#78909C", "#99ba78", "#24A99C", "#2E6657"))
color_palette
show_col(color_palette %>% pull(hex))

# add color_bin and color
chart_data <- data %>% mutate(color_bin = group_number,
                              color = case_when(color_bin == "1" ~ color_palette %>% slice(1) %>% pull(hex),
                                                color_bin == "2" ~ color_palette %>% slice(2) %>% pull(hex),
                                                color_bin == "3" ~ color_palette %>% slice(3) %>% pull(hex),
                                                color_bin == "4" ~ color_palette %>% slice(4) %>% pull(hex),
                                                color_bin == "5" ~ color_palette %>% slice(5) %>% pull(hex),
                                                color_bin == "6" ~ color_palette %>% slice(6) %>% pull(hex),
                                                color_bin == "7" ~ color_palette %>% slice(7) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#/////////////////////


# create bar chart ####
pandem_bar_chart <- chart_data %>% ggplot(data = ., aes(x = fct_reorder(.f = factor(name), .x = value_1), 
                                                        y = value_1, 
                                                        fill = factor(color_bin, 
                                                                       levels = c("1", "2", "3", "4", "5", "6", "7")))) +        
        geom_col() + 
        scale_fill_manual(values = chart_data_color_list) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(0, 1.1), expand = c(0, 0)) +
        labs(x = NULL, y = "Pandemic Democratic Violations Index", 
             title = NULL,
             caption = NULL, fill = "") +
        # coord_fixed(ratio = 1/.05, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_line(color = "#DDDDDD"),
                panel.grid.major.y = element_blank(),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#595959"),
                axis.line.y.left = element_line(color = "#595959"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", 
                                            margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", 
                                            margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5)
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) + 
        coord_flip()


# inspect
pandem_bar_chart


#////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////


# get time series chart_data ####
chart_data <- tibble(group_number = c(rep(1, times = 10),
                               rep(2, times = 10),
                               rep(3, times = 10),
                               rep(4, times = 10),
                               rep(5, times = 10),
                               rep(6, times = 10),
                               rep(7, times = 10)),
                     year = rep(seq(from = 2011, to = 2020, by = 1), times = 7),
                     value = sample(seq(from = 1, to = 5), size = 70, replace = TRUE))
chart_data
chart_data %>% count(group_number)


#/////////////////////////


# blue_grey_green
color_palette <- tibble(hex = c("#08306B", "#2171B5", "#6BAED6", "#78909C", "#99ba78", "#24A99C", "#2E6657"))
color_palette
show_col(color_palette %>% pull(hex))

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = group_number,
                              color = case_when(color_bin == "1" ~ color_palette %>% slice(1) %>% pull(hex),
                                                color_bin == "2" ~ color_palette %>% slice(2) %>% pull(hex),
                                                color_bin == "3" ~ color_palette %>% slice(3) %>% pull(hex),
                                                color_bin == "4" ~ color_palette %>% slice(4) %>% pull(hex),
                                                color_bin == "5" ~ color_palette %>% slice(5) %>% pull(hex),
                                                color_bin == "6" ~ color_palette %>% slice(6) %>% pull(hex),
                                                color_bin == "7" ~ color_palette %>% slice(7) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#/////////////////////


# create chart
pandem_panback_line_chart <- chart_data %>% 
        ggplot(data = ., aes(x = year, y = value, 
                             color = factor(color_bin, 
                                            levels = c("1", "2", "3", "4", "5", "6", "7")))) +
        geom_line(size = 3) + 
        geom_point(size = 6) + 
        scale_color_manual(values = chart_data_color_list) +
        # scale_x_discrete(expand = c(0, 0)) +
        # scale_y_continuous(breaks = seq(from = 0, to = .4, by = .1), limits = c(-.05, .45), expand = c(-.05, 0)) +
        # scale_x_continuous(breaks = seq(from = 0, to = .6, by = .1), limits = c(-.05, .65), expand = c(-.05, 0)) +
        labs(x = "Pandemic Democratic Violations Index", y = "Pandemic Backsliding Index", 
             title = NULL,
             caption = NULL, color = "") +
        # coord_fixed(ratio = 1/.05, clip = "off") +
        # coord_flip() + 
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 13, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_blank(),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#595959"),
                axis.line.y.left = element_line(color = "#595959"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", 
                                            margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", 
                                            margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 18, face = "bold", hjust = .5, family = "Calibri", color = "#000000", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5)
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 


# inspect
pandem_panback_line_chart


#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# create two color bar chart ####

# blue_grey_green
color_palette <- tibble(hex = c("#08306B", "#2171B5", "#6BAED6", "#78909C", "#99ba78", "#24A99C", "#2E6657"))
color_palette
show_col(color_palette %>% pull(hex))

# palette supports 7 colors, plus possible extensions via fill/line type
show_col(color_palette %>% slice(1, 3) %>% pull(hex)) # 2 colors
show_col(color_palette %>% slice(1, 2, 3) %>% pull(hex)) # 3 colors
show_col(color_palette %>% slice(1, 2, 3, 4) %>% pull(hex)) # 4 colors
show_col(color_palette %>% slice(1, 2, 3, 6, 7) %>% pull(hex)) # 5 colors
show_col(color_palette %>% slice(1, 2, 3, 5, 6, 7) %>% pull(hex)) # 6 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7) %>% pull(hex)) # 7 colors

# add color_bin and color
chart_data <- data %>% mutate(color_bin = group_number,
                              color = case_when(color_bin == "1" ~ color_palette %>% slice(1) %>% pull(hex),
                                                color_bin == "2" ~ color_palette %>% slice(2) %>% pull(hex),
                                                color_bin == "3" ~ color_palette %>% slice(3) %>% pull(hex),
                                                color_bin == "4" ~ color_palette %>% slice(4) %>% pull(hex),
                                                color_bin == "5" ~ color_palette %>% slice(5) %>% pull(hex),
                                                color_bin == "6" ~ color_palette %>% slice(6) %>% pull(hex),
                                                color_bin == "7" ~ color_palette %>% slice(7) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////


pandem_bar_chart <- chart_data %>% 
        filter(group_number %in% c("1", "2", "3", "5", "6", "7")) %>%
        ggplot(data = ., aes(x = fct_reorder(.f = factor(name), .x = value_1), 
                                                        y = value_1, 
                                                        fill = factor(color_bin))) +        
        geom_col() + 
        scale_fill_manual(values = chart_data_color_list) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(0, 1.1), expand = c(0, 0)) +
        labs(x = NULL, y = "Pandemic Democratic Violations Index", 
             title = NULL,
             caption = NULL, fill = "") +
        # coord_fixed(ratio = 1/.05, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_line(color = "#DDDDDD"),
                panel.grid.major.y = element_blank(),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", 
                                           margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#595959"),
                axis.line.y.left = element_line(color = "#595959"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", 
                                            margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 12, color = "#000000", 
                                            margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 12, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5)
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) + 
        coord_flip()


# inspect
pandem_bar_chart

