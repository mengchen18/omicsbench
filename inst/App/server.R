library(shiny)
library(omicsbench)

annot_dir <- "~/"
project_dir <- c(project = "~/")
lan <- "en"

server <- function(input, output, session) {
  omicsbench::cruncher_server( "l0", annot_dir = annot_dir, project_dir = project_dir)
}
