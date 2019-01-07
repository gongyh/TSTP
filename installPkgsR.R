#!/usr/bin/Rscript

# Check and install if the package is not installed and then load them into the R session.

instpkg <- function(pkg,repo){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg) && repo == 'CRAN') {
    install.packages(new.pkg, dependencies = TRUE, repos='https://cloud.r-project.org/')
  }
  sapply(pkg, require, character.only = TRUE)
}


# CRAN R packages
CRANpkgs <- c("shiny", "shinyBS", "shinydashboard", "shinycssloaders", "shinyjs",
              "DT", "formattable", "data.table", "devtools", "dtwclust", "doParallel",
              "ggplot2", "reshape2","dplyr")

instpkg(CRANpkgs, "CRAN")
