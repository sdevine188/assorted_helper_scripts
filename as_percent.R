# as_percent converts decimals to percentages, rounding percentage to two decimal points, 
# also ensuring that two decimal points are retained even if percentage point is whole number (e.g. 34.00%)
as_percent <- function(decimal, digits = 2) {
        str_trim(str_c(format(round(decimal * 100, digits = digits), nsmall = digits), "%", sep = ""))
}

# as_percent(.3595)
# as_percent(.3696834)
# as_percent(.34)
# as_percent(.3401)
# as_percent(.3409)
# as_percent(.34019)
# as_percent(.34012)


