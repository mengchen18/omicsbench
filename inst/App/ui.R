library(shiny)
library(omicsbench)

annot_dir <- "~/"
project_dir <- c(project = "~/")
lan <- "en"

ui <- fluidPage(
  omicsbench::cruncher_ui( "l0")
)