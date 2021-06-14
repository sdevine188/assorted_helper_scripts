library(tidyverse)
library(jcolors)
library(viridis)
library(scales)
library(RColorBrewer)
library(pals)
library(colorspace)
library(colorRamps)
library(ggrepel)
library(Polychrome)

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

# a good cheatsheet on R color palettes
# https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf


#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# convert rgb color values to hex color
# helpful when getting font color from a word doc (or pdf converted to word) and it just gives rbg

# https://gist.github.com/mbannert/e9fcfa86de3b06068c83

convert_color_from_rgb_to_hex <- function(r, g, b) {
        rgb(r, g, b, maxColorValue = 255)
}


#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# create random but distinctive polychrome_palette of any length
# https://cran.r-project.org/web/packages/Polychrome/vignettes/creatingPalettes.html
polychrome_palette <- createPalette(N = 48, seedcolors = c("#000000"), M = 50000)
polychrome_palette
polychrome_palette %>% swatch()

starwars %>% add_count(homeworld) %>% ggplot(data = ., mapping = aes(x = fct_reorder(.f = homeworld, .x = n, .desc = FALSE), 
                                                                     y = n, fill = homeworld)) +
        geom_col() +
        scale_fill_manual(values = unname(polychrome_palette))


#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# create color_palette ####

# final pick is blue_grey_green, with yellow and purple as extra colors when needed
# note that priority of use will be blues, then greys, and then greens (with yellow and purples last)
# us web design system advocates blue_grey palette as 508 compliant contrast, 
# https://v1.designsystem.digital.gov/components/colors/
# though my palette takes the blues and greys from R Color Brewer
# and us .gov data visualization standards explicitly recommends R Color Brewer as a best practice for 508 compliant contrast
# https://xdgov.github.io/data-design-standards/components/colors

color_palette <- tibble(hex = c("#083D7F", "#2474B6", "#8BBFD0",
                                "#CBCBCB", "#7D7D7D",
                                "#99ba78", "#35B779FF", "#006629", 
                                "#E4DC68", "#FA8533", "#E5590A", "#DD2A24",
                                "#8B008B", "#DA70D6"))
color_palette %>% pull(hex) %>% show_col()

# color_palette supports 11 colors, plus possible extensions via fill/line type
show_col(color_palette %>% slice(1, 3) %>% pull(hex)) # 2 colors
show_col(color_palette %>% slice(1, 2, 3) %>% pull(hex)) # 3 colors
show_col(color_palette %>% slice(1, 2, 3, 4) %>% pull(hex)) # 4 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5) %>% pull(hex)) # 5 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6) %>% pull(hex)) # 6 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7) %>% pull(hex)) # 7 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8) %>% pull(hex)) # 8 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8, 9) %>% pull(hex)) # 9 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) %>% pull(hex)) # 10 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) %>% pull(hex)) # 11 colors


#//////////////////////////////////////////////////////////////////////////////////


# scratchpad to inspect alternative colors

# inspect blues ####
brewer.blues(10) %>% show_col()
brewer.blues(20) %>% show_col()

# final selection is drawn from brewer.blues(20) %>% show_col()
# #083D7F is col 4, row 4
# #2474B6 is col 5, row 3
# #8BBFD0 is col4, row 2
c("#08306B", "#2474B6", "#8BBFD0") %>% show_col()
c("#083D7F", "#2474B6", "#8BBFD0") %>% show_col()


#/////////////////////


# inspect greys ####
brewer.greys(10) %>% show_col()
brewer.greys(20) %>% show_col()
brewer.greys(30) %>% show_col()

# note the final selection is "#BEBEBE", "#636363"
# "#CBCBCB" is drawn from brewer.greys(30) %>% show_col(), col 4, row 2
# "#7D7D7D" is drawn from brewer.greys(30) %>% show_col(), col 6, row 3
# historic: "#919191" is drawn from brewer.greys(30) %>% show_col(), col 4, row 3
# historic: "#585858" is drawn from brewer.greys(30) %>% show_col(), col 4, row 4
c("#CBCBCB", "#7D7D7D") %>% show_col()


#//////////////////


# inspect greens ####
# "#35B779FF" is from show_col(viridis_pal()(10)), col 3, row 2
# "#006629" is from brewer.greens(20) %>% show_col(), col 3, row 4
# "#E4DC68" was from colorspace::heat_hcl(12) %>% show_col(), col 3, row 3
# "#99ba78" not sure where i got it???
c("#E4DC68", "#99ba78", "#35B779FF", "#006629") %>% show_col()


viridis_pal()(10)
show_col(viridis_pal()(10))
show_col(viridis_pal()(20))
primary.colors(30) %>% show_col()
heat_hcl(12) %>% show_col()
brewer.greens(10) %>% show_col()
brewer.greens(20) %>% show_col()
# https://www.rapidtables.com/web/color/green-color.html

display_jcolors("pal9")
jcolors("pal9")
display_jcolors("pal7")
jcolors("pal7")
display_jcolors("pal5")
jcolors("pal5")
display_jcolors("pal6")
jcolors("pal6")
c("#93C572", "#99ba78") %>% show_col()
terrain_hcl(12, c = c(65, 0), l = c(45, 95), power = c(1/3, 1.5)) %>% show_col()
terrain_hcl(12, h = c(130, 43), c = 100, l = c(70, 90)) %>% show_col()
x <- colorRampPalette(c("olivedrab3"))
x(1)
brewer.piyg(10) %>% show_col()


#//////////////////


# inspect oranges/red ####
# https://www.rapidtables.com/web/color/purple-color.html
# #FA8533 from brewer.oranges(20), row 3, col 1
# E5590A from brewer.oranges(20), row 3, col 4
# DD2A24 from brewer.reds(20), row 3, col 4
brewer.oranges(20) %>% show_col()
brewer.reds(20) %>% show_col()
c("#FA8533", "#E5590A", "#DD2A24") %>% show_col()


#//////////////////


# inspect purples ####
# https://www.rapidtables.com/web/color/purple-color.html
# #8B008B and #DA70D6 are from rapidtables.com
viridis_pal()(10)
show_col(viridis_pal()(20))
brewer.purples(20) %>% show_col()
c("#8B008B", "#7A378B", "#DA70D6") %>% show_col()


#////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////



# create data ####
data <- tibble(group_number = as.character(c(rep(seq(from = 1, to = 5, by = 1), times = 14))),
               name = starwars %>% slice(1:70) %>% pull(name),
               value_1 = sample(seq(from = .1, to = 1, by = .1), size = 70, replace = TRUE),
               value_2 = sample(seq(from = .1, to = 1, by = .1), size = 70, replace = TRUE),
               measure_1 = sample(seq(from = .1, to = 1, by = .1), size = 70, replace = TRUE),
               measure_2 = sample(seq(from = .1, to = 1, by = .1), size = 70, replace = TRUE),
               measure_3 = sample(seq(from = .1, to = 1, by = .1), size = 70, replace = TRUE),
               measure_4 = sample(seq(from = .1, to = 1, by = .1), size = 70, replace = TRUE)) %>%
        arrange(group_number) %>%
        mutate(color_group = as.character(c(rep(seq(from = 1, to = 14, by = 1), times = 5))))

# inspect
data
data %>% count(group_number)
data %>% count(group_number, name)
data %>% count(color_group) %>% arrange(as.numeric(color_group))
data %>% print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////////////////////////


# create chart_data ####
chart_data <- data %>% mutate(color_bin = color_group,
                              color = case_when(color_bin == "1" ~ color_palette %>% slice(1) %>% pull(hex),
                                                color_bin == "2" ~ color_palette %>% slice(2) %>% pull(hex),
                                                color_bin == "3" ~ color_palette %>% slice(3) %>% pull(hex),
                                                color_bin == "4" ~ color_palette %>% slice(4) %>% pull(hex),
                                                color_bin == "5" ~ color_palette %>% slice(5) %>% pull(hex),
                                                color_bin == "6" ~ color_palette %>% slice(6) %>% pull(hex),
                                                color_bin == "7" ~ color_palette %>% slice(7) %>% pull(hex),
                                                color_bin == "8" ~ color_palette %>% slice(8) %>% pull(hex),
                                                color_bin == "9" ~ color_palette %>% slice(9) %>% pull(hex),
                                                color_bin == "10" ~ color_palette %>% slice(10) %>% pull(hex),
                                                color_bin == "11" ~ color_palette %>% slice(11) %>% pull(hex),
                                                color_bin == "12" ~ color_palette %>% slice(12) %>% pull(hex),
                                                color_bin == "13" ~ color_palette %>% slice(13) %>% pull(hex),
                                                color_bin == "14" ~ color_palette %>% slice(14) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

# inspect
chart_data


#/////////////////////////////////////////////////////////////////////////////////////////////////////////


# create stacked_bar_chart ####
stacked_bar_chart <- chart_data %>% 
        # filter(group_number %in% c("1", "4")) %>%
        ggplot(data = ., mapping = aes(x = group_number, y = value_1, fill = color_bin)) +
        geom_bar(position = position_stack(), stat = "identity", width = .9) +
        scale_fill_manual(values = chart_data_color_list) +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                # panel.grid.major.x = element_line(color = "#DDDDDD"),
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
        )
        
# inspect
stacked_bar_chart
        
        
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# create scatterplot ####
scatterplot <- chart_data %>% 
        ggplot(data = ., aes(x = value_1, y = value_2, label = name,
                             color = factor(color_bin, 
                        levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")))) +
        geom_point(size = 6) + 
        geom_text_repel(fontface = "bold", point.padding = .2, size = 4) +
        scale_color_manual(values = chart_data_color_list) +
        # scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.01, 1.01)) +
        scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(-.01, 1.01)) +
        labs(x = "test", y = "test", 
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
                # panel.grid.major.x = element_line(color = "#DDDDDD"),
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
        )


# inspect
scatterplot


#/////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////


# create jitter_plot ####
jitter_plot <- chart_data %>% filter(group_number %in% c(1, 2, 3, 4)) %>% 
        ggplot(data = ., aes(x = group_number, 
                                        y = value_1, color = factor(color_bin, 
                        levels = c("1", "2", "3", "4", "5", "6","7", "8", "9", "10", "11")))) +  
        geom_jitter(width = 0.15, height = .25, size = 6) +         
        scale_size(guide = "none") +
        scale_color_manual(values = chart_data_color_list) +
        scale_x_discrete(expand = c(.1, 0)) +
        scale_y_discrete(expand = c(.1, 0)) +
        # scale_x_continuous(breaks = seq(from = 0, to = .8, by = .1), limits = c(-.05, .85), expand = c(-.05, 0)) +
        # scale_y_continuous(breaks = seq(from = 0, to = .6, by = .1), limits = c(-.05, .65), expand = c(-.05, 0)) +
        labs(x = "test", y = "test",
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
        guides(colour = guide_legend(override.aes = list(size = 4)))

# inspect
jitter_plot


#/////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////


# create bar chart ####
bar_chart <- chart_data %>% ggplot(data = ., aes(x = fct_reorder(.f = factor(name), .x = value_1), 
                                                        y = value_1, 
                                                        fill = factor(color_bin, 
                        levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")))) +        
        geom_col() + 
        scale_fill_manual(values = chart_data_color_list) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(0, 1.1), expand = c(0, 0)) +
        labs(x = NULL, y = "test", 
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
bar_chart


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
                               rep(7, times = 10),
                               rep(8, times = 10),
                               rep(9, times = 10),
                               rep(10, times = 10),
                               rep(11, times = 10),
                               rep(12, times = 10),
                               rep(13, times = 10),
                               rep(14, times = 10)),
                     year = rep(seq(from = 2011, to = 2020, by = 1), times = 14),
                     value = sample(seq(from = 1, to = 5), size = 140, replace = TRUE))
chart_data
chart_data %>% count(group_number)


#/////////////////////////


# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = group_number,
                              color = case_when(color_bin == "1" ~ color_palette %>% slice(1) %>% pull(hex),
                                                color_bin == "2" ~ color_palette %>% slice(2) %>% pull(hex),
                                                color_bin == "3" ~ color_palette %>% slice(3) %>% pull(hex),
                                                color_bin == "4" ~ color_palette %>% slice(4) %>% pull(hex),
                                                color_bin == "5" ~ color_palette %>% slice(5) %>% pull(hex),
                                                color_bin == "6" ~ color_palette %>% slice(6) %>% pull(hex),
                                                color_bin == "7" ~ color_palette %>% slice(7) %>% pull(hex),
                                                color_bin == "8" ~ color_palette %>% slice(8) %>% pull(hex),
                                                color_bin == "9" ~ color_palette %>% slice(9) %>% pull(hex),
                                                color_bin == "10" ~ color_palette %>% slice(10) %>% pull(hex),
                                                color_bin == "11" ~ color_palette %>% slice(11) %>% pull(hex),
                                                color_bin == "12" ~ color_palette %>% slice(12) %>% pull(hex),
                                                color_bin == "13" ~ color_palette %>% slice(13) %>% pull(hex),
                                                color_bin == "14" ~ color_palette %>% slice(14) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#//////////////////////////////////////////////////////////////////////////////////////////////////


# create chart
line_chart <- chart_data %>% 
        ggplot(data = ., aes(x = year, y = value, 
                             color = factor(color_bin, 
                                levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")))) +
        geom_line(size = 2) + 
        geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        # scale_x_discrete(expand = c(0, 0)) +
        # scale_y_continuous(breaks = seq(from = 0, to = .4, by = .1), limits = c(-.05, .45), expand = c(-.05, 0)) +
        # scale_x_continuous(breaks = seq(from = 0, to = .6, by = .1), limits = c(-.05, .65), expand = c(-.05, 0)) +
        labs(x = "test", y = "test", 
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
line_chart


#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////

