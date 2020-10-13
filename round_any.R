library(tidyverse)

# create round_any function to round to specific whole numbers
# note plyr has this function too, but not dplyr, and stack overflow cautioned about function  name collisions when loading plyr
# so they just created this function as a standalone without loading plyr
# https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr/46489816#46489816

round_any <- function(x, accuracy, f = round) {
        return(f(x / accuracy) * accuracy)
}


# test
# note these tests are from the plyr round_any docs
# round_any(135, 10)
# round_any(135, 100)
# round_any(135, 25)
# round_any(135, 10, floor)
# round_any(135, 100, floor)
# round_any(135, 25, floor)
# round_any(135, 10, ceiling)
# round_any(135, 100, ceiling)
# round_any(135, 25, ceiling)