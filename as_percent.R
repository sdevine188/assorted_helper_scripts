# # load as_percent()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("as_percent.R")
# setwd(current_wd)


# note that base round() will round .5 "to the even digit" according to some IEC standard (see ?round)
# e.g round(x = .305, digits = 1) = .3 instead of .31
# so this homemade round2 function from stack overflow fixes that
# https://stackoverflow.com/questions/12688717/round-up-from-5
round2 = function(x, digits) {
        sign = sign(x)
        z = abs(x) * 10^digits
        z = z + 0.5
        z = trunc(z)
        z = z / 10^digits
        z * sign
}


# as_percent converts decimals to percentages, rounding percentage to two decimal points, 
# also ensuring that two decimal points are retained even if percentage point is whole number (e.g. 34.00%)
as_percent <- function(decimal, digits = 2) {
        str_trim(str_c(format(round2(decimal * 100, digits = digits), nsmall = digits), "%", sep = ""))
}

# as_percent(.3595)
# as_percent(.3696834)
# as_percent(.34)
# as_percent(.3401)
# as_percent(.3405)
# as_percent(.3405, digits = 1)
# as_percent(.34019)
# as_percent(.34012)


