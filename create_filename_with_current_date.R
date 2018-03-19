current_date <- Sys.Date()
current_date <- str_replace_all(current_date, pattern = "-", replacement = "")
filename <- str_c("treatment", "_", current_date, ".csv")
filename