# # load ggplot_center_legend_title function
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("ggplot_center_legend_title.R")
# setwd(current_wd)


library(tidyverse)
library(grid)
library(gtable)

# create ggplot_center_legend_title function to center long legend titles
# see stackoverflow post for example of problem and the solution, which I'm just wrapping
# https://stackoverflow.com/questions/48000292/center-align-legend-title-and-legend-keys-in-ggplot2-for-long-legend-titles

# note that the solution involves converting the ggplot object into a grob
# so the output is not a ggplot object and you can't add more layers
# so ggplot_center_legend_title() should be the last step in the plot-creating process
# you can print it in the viewer with grid.draw(), and you can still save it with ggsave() as usual

ggplot_center_legend_title <- function(plot) {
        
        # extract legend
        g <- ggplotGrob(plot)
        grobs <- g$grobs
        legend_index <- which(sapply(grobs, function(x) x$name) == "guide-box")
        legend <- grobs[[legend_index]]

        # extract guides table
        guides_index <- which(sapply(legend$grobs, function(x) x$name) == "layout")
        guides <- legend$grobs[[guides_index]]

        # add extra column for spacing
        # guides$width[5] is the extra spacing from the end of the legend text
        # to the end of the legend title. If we instead distribute it 50:50 on
        # both sides, we get a centered legend
        guides <- gtable_add_cols(guides, 0.5*guides$width[5], 1)
        guides$widths[6] <- guides$widths[2]
        title_index <- guides$layout$name == "title"
        guides$layout$l[title_index] <- 2
        
        # reconstruct legend and write back
        legend$grobs[[guides_index]] <- guides
        g$grobs[[legend_index]] <- legend 
        
        # return grob
        return(g)
}


###############


# # test
# plot <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Petal.Width)) + 
#         geom_point(size = 3) +
#         scale_color_distiller(palette = "YlGn", type = "seq", direction = -1,
#                               name = "Long legend heading\nShould be centered") +
#         theme(legend.title.align = 0.5)
# plot
# 
# plot_centered <- ggplot_center_legend_title(plot = plot)
# grid.newpage()
# grid.draw(plot_centered)
# ggsave(filename = "plot_centered.pdf", plot = plot_centered, units = "in", dpi = 300)

