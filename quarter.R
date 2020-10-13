# lubridate's quarter function

quarter <- function(x, with_year = FALSE, fiscal_start = 1) {
        fs <- fiscal_start - 1
        shifted <- seq(fs, 11 + fs) %% 12 + 1
        m <- month(x)
        quarters <- rep(1:4, each = 3)
        s <- match(m, shifted)
        q <- quarters[s]
        if (with_year) {
                uq <- quarters[m]
                inc_year <- q == 1 & uq == 4
                year(x) + inc_year + q/10
        }
        else q
}