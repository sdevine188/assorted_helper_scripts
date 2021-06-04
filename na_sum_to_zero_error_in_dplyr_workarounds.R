# there is an NA-sum-to-zero error when summing all NA values in dplyr, since it returns zero, instead of NA
# so if you run a sum on unknown data, you can't trust whether 
# an output sum of zero resulted from actual zero values, or actual NA values
# https://stat.ethz.ch/pipermail/r-help/2002-April/020794.html


# this shows the NA-sum-to-zero error when using mutate, compared to a base r sum of an NA vector
sum(c(NA, NA, NA))
tibble(x = c(NA, NA, NA)) %>% mutate(sum = sum(x, na.rm = TRUE))

# this shows the NA-sum-to-zero error when using summarize, compared to a base r sum of an NA vector
sum(c(NA, NA, NA))
tibble(x = c(1, NA, NA),
       group = c("a", "b", "b")) %>% group_by(group) %>% summarize(sum = sum(x, na.rm = TRUE))


#////////////////////////////////////////////


# solution is to always use sumNA from the bazar package instead of sum
# https://www.rdocumentation.org/packages/bazar/versions/1.0.11/topics/sumNA
# https://github.com/paulponcet/bazar
# https://cran.r-project.org/web/packages/bazar/index.html
x <- c(NA, NA)
sum(x)
sumNA(x)
sum(x, na.rm = TRUE)
sumNA(x, na.rm = TRUE) # here is the difference with 'sum()'

# still returns 0 if you sum a completely empty set
sum(c())
sumNA(c())  


# contents of sumNA
# > sumNA
# function (..., na.rm = FALSE) 
# {
#         x <- unlist(list(...))
#         if (na.rm && length(x) && all(is.na(x))) 
#                 return(x[1] + NA)
#         sum(x, na.rm = na.rm)
# }
# <bytecode: 0x000001a2c1124428>
#         <environment: namespace:bazar>










