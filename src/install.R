
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c(
  "googlesheets4",
  "dplyr",
  "purrr",
  "forecast",
  "ggplot2",
  "cowplot",
  "ggrepel",
  "tidyr"
)

ipak(packages)