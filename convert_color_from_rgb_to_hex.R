# # load convert_color_from_rgb_to_hex()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("convert_color_from_rgb_to_hex.R")
# setwd(current_wd)




# convert rgb color values to hex color
# helpful when getting font color from a word doc (or pdf converted to word) and it just gives rbg

# https://gist.github.com/mbannert/e9fcfa86de3b06068c83

convert_color_from_rgb_to_hex <- function(r, g, b) {
        rgb(r, g, b, maxColorValue = 255)
}