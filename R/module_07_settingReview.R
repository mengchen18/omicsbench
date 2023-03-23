module_paramReview <- function(id, obj) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$setting <- renderUI({
        req(obj())
        procConfigYaml(obj())
      })
      
      output$mmtext <- renderUI({
        req(obj())
        methodText(obj())
      })
      
      eventReactive(input$save, {
        req(obj())
        show_modal_spinner()
        rl <- prepESVObj(obj())
        remove_modal_spinner()
        rl
      })
    }
  )
}

module_paramReview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    absolutePanel(top = -10, left = 275, tags$h2(.msg$review_title[[lan]]), style = "z-index: 1111;"),
    absolutePanel(top = 5, right = 20, actionButton(inputId = ns("save"), label = .msg$review_confirm[[lan]]), style = "z-index: 1111;" ),
    fluidRow(
      column(
        6, 
        wellPanel(
          style = "background: white; height: 800px",
          tags$h3(.msg$paramset[[lan]]),
          uiOutput(ns("setting"))
        )
      ),
      column(
        6, 
        wellPanel(
          style = "background: white; height: 800px",
          tags$h3(.msg$review_methods[[lan]]),
          uiOutput(ns("mmtext"))
        )
      )
    )
  )
}

# 
###### 
# library(shiny)
# a <- readRDS("~/projects/SXLj_GtNH9g0yKFCq5f2wY3NqCrBVhpR5uQB/P03/obj.RDS")
# attr(a, "S3_normalization")
# 
# ui <- fluidPage(
#   module_paramReview_ui("test")
# )
# 
# server <- function(input, output, session) {
#   qq <- module_paramReview("test", obj = reactive(a))
#   observe({
#     print( attr(qq(), "S5_misc") )
#   })
# }
# shinyApp(ui, server)
