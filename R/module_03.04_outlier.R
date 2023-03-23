module_outlier <- function(id, esv) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns 
      
      observeEvent(input$settingOrHelp, {
        updateTabsetPanel(session, "settingOrHelp_tabs", selected = paste0("tab_", input$settingOrHelp))
      })
      
      eventReactive(input$save, {
        show_modal_spinner(text = .msg$calculating[[lan]])
        at <- attr(esv(), "S6.2_outlier")
        dd <- esv()
        if (is.null(at) || !at) {
          fd <- fData(dd)
          gs <- attr(fd, "GS")
          o_up <- getOutliersUp(exprs(dd))
          colnames(o_up) <- paste("Outlier|Up", colnames(o_up), sep = "|")
          o_down <- getOutliersDown(exprs(dd))
          colnames(o_down) <- paste("Outlier|Down", colnames(o_down), sep = "|")
          nfd <- cbind(fd, o_up, o_down)
          attr(nfd, "GS") <- gs
          fData(dd) <- nfd
          attr(dd, "S6.2_outlier") <- TRUE
        }
        attr(dd, "fx") <- paste("Outlier|Up|sample", sep = "|")
        attr(dd, "fy") <- paste("Outlier|Up|fold.change.log10", sep = "|")
        saveRDS(dd, file = attr(dd, "filePath"))
        remove_modal_spinner()
        dd
      })
    }
  )
}

module_outlier_ui <- function(id) {
  ns <- NS(id)
  tagList(
    absolutePanel(top = -10, left = 275, tags$h2(.msg$outlier_title[[lan]]), style = "z-index: 1111;"),
    absolutePanel(top = 5, right = 20, actionButton(inputId = ns("save"), label = .msg$analyzecheck[[lan]]), style = "z-index: 1111;" ),
    absolutePanel(
      top = 20, right = 165, style = "z-index: 1111;",
      radioGroupButtons(
        inputId = ns("settingOrHelp"), label = NULL, size = "xs",
        choices = structure(c("help"), names = .msg$helpmanu[[lan]]),
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
    ),
    tabsetPanel(
      id = ns("settingOrHelp_tabs"),
      type = "hidden",
      tabPanelBody(
        "tab_help",
        wellPanel(
          style = "background: white; height: 800px", 
          img(src='doc_outlier.png', height="750px", 
              style="display: block; margin-left: auto; margin-right: auto;")
        )
      )
    )
  )
}
