# how to install r packages from cran tar.gz, cran binary, or github .zip

# one option that has worked when even cran downloads fail due to weird NAMESPACE-type errrors
# is to create github repo with personal computer, upload library, then download as zip -> unzip/extract -> and drag/drop folder
# this worked with r_packages_flextable repo for instance

# note i've had trouble with r recognizing rtools 3.5.1
# the link below has code to run that adds rtools to the path environment for that r session, but needs to be run each session
# this allows me to install the tar.gz files though
# https://stackoverflow.com/questions/33103203/rtools-is-not-being-detected-from-rstudio
Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))
Sys.setenv(BINPREF = "C:/Rtools/mingw_$(WIN)/bin/")

# cran tar.gz - this is the best method now that the firewall appears to be stopping install.packages even when off-vpn
# you copy the zipped tar.gz file into the R libraries folder, set the working directory to the same folder, then run the code below
# This tar.gz method is from StackOverflow (except I had to add ".gz" to the file name to get it to work for me).  
setwd("~/R/win-library/3.5")
install.packages("package_name.tar.gz", repos = NULL, type = "source")

# cran binary zip
# just unzip the file and copy to r libraries folder

# github .zip
# https://stackoverflow.com/questions/17366772/install-r-packages-from-github-downloading-master-zip
# download the master.zip file from github (should be green “Clone or download” button/menu)
# save unzipped folder in R libraries folder
# then run code below:
library(devtools)
install("path/to/causalTree-master")


# old alternative, but looks like source_pkg is unnecessary/defunct??
# https://stackoverflow.com/questions/33179156/installing-a-package-offline-from-github
# download the master.zip file from github (should be green “Clone or download” button/menu)
# save zipped folder in R libraries folder
# then run code below:
# library(devtools)
# source <- devtools:::source_pkg("causalTree-master")
# install(source)