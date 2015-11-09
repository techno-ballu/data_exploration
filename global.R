# Any code in this file is guaranteed to be called before either
# ui.R or server.R

# library(df2json)

# istall latest data.table to access fread function!
#   devtools::install_github("Rdatatable/data.table", build_vignettes = FALSE)
#   library(data.table) # using the development version!
#   install.packages("data.table")

list.of.packages <- c("shiny", "googleVis", "gdata", "corrplot", "caret", "xlsx", "FSelector", "data.table", "markdown", 
                      "R.oo", "rjson", "devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}

shinyBS.package <- c("shinyBS")
if (!shinyBS.package %in% installed.packages()) {
  require("devtools")
  install_github("shinyBS", "Bolaka")
}

list.of.packages <- c(list.of.packages, shinyBS.package)
lapply(list.of.packages, require, character.only = TRUE)

source("nvd3.R")
source("d3.R")
source("widgets.R")
# insertSource("bsModal3.R", package = "shinyBS", functions = "bsModal")