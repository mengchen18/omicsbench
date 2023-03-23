#' @importFrom stats sd
module_cortest <- function(id, esv) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns 
      
      ptab <- reactive({
        req(esv())
        getOriginDf( pData(esv()) )
      })
      # 
      output$phenoTab <- DT::renderDT({
        req(ptab())
        dt <- formatDTScrollY( ptab(), height = "635px" )
        if (!is.null(input$corcol) && any(input$corcol %in% colnames(ptab()))) {
          dt <- formatStyle(dt, input$corcol, backgroundColor = "#85C1E9")
        }
        dt
      }, server = FALSE)
      # 
      numcols <- reactive({
        req(ptab())
        i <- sapply(ptab(), function(x) {
          if (!is.numeric(x) || sum(!is.na(x)) < 4 )
            return(FALSE)
          sd(x, na.rm = TRUE) > 0
        })
        colnames(ptab())[i]
      })
      # 
      observe({
        req(numcols())
        updateMultiInput(session, inputId = "corcol", choices = numcols(), selected = NULL)
      })
      # 
      observeEvent(input$add, {
        updateMultiInput(
          session, inputId = "corcol", choices = numcols(), selected = numcols()[ grep(input$mulcorcol, numcols()) ]
        )
      })
      # 
      observe({
        req(numcols())
        req(v <- attr(esv(), which = "S6.3_cortest"))
        updateMultiInput(
          session, inputId = "corcol", choices = numcols(), selected = v
        )
      })
      
      observeEvent(input$settingOrHelp, {
        updateTabsetPanel(session, "settingOrHelp_tabs", selected = paste0("tab_", input$settingOrHelp))
      })
      # 
      eventReactive(input$save, {
        
        i <- input$corcol
        maxN <- 50
        
        if (length(i) == 0)
          return(NULL)
        
        if (length(i) > maxN) {
          showModal(modalDialog(
            sprintf(.msg$cort_max[[lan]], maxN),
            title = .msg$somethingwrong[[lan]],
            footer = modalButton(.msg$ok[[lan]])
          ))
          return(NULL)
        }
        
        # otherwise calculate
        show_modal_spinner(text = .msg$calculating[[lan]])
        rl <- esv()
        # if parameter doesn't change, don't do anything
        var <- setdiff(i, attr(esv(), "S6.3_cortest"))
        var <- intersect(var, colnames(ptab()))
        if ( length(var) > 0 ) {
          fdata <- getOriginDf( fData(esv()))
          expr <- exprs(esv())
          d0 <- ptab()[, var, drop = FALSE]
          c1 <- correlationAnalysis(expr, d0, min.value = 5)
          c2 <- correlationAnalysis(fillNA(expr), d0, min.value = 5)
          if (ncol(c2) > 0)
            colnames(c2) <- paste0(colnames(c2), ".impute")
          fd <- fData(rl)
          gs <- attr(fd, "GS")
          cc <- cbind(c1, c2)
          for (ii in colnames(cc))
            fd[[ii]] <- cc[, ii]
          attr(fd, "GS") <- gs
          fData(rl) <- fd
          attr(rl, "S6.3_cortest") <- i
        }
        attr(rl, "fx") <- paste("Cor", i[1], "R", sep = "|")
        attr(rl, "fy") <- paste("Cor", i[1], "logP", sep = "|")
        saveRDS(rl, file = attr(rl, "filePath"))
        remove_modal_spinner()
        rl
      })
    }
  )
}

module_cortest_ui <- function(id, tabName) {
  
  ns <- NS(id)
  tagList(
    tags$style(HTML(sprintf("
#shiny-tab-%s .multi-wrapper {
        height: 642px;
}
#shiny-tab-%s .non-selected-wrapper {
        height: 600px;
}
#shiny-tab-%s .selected-wrapper {
        height: 600px;
}", tabName, tabName, tabName))),
    absolutePanel(top = -10, left = 275, tags$h2(.msg$cort_title[[lan]]), style = "z-index: 1111;"),
    absolutePanel(top = 5, right = 20, style = "z-index: 1111;", actionButton(inputId = ns("save"), 
      label = .msg$analyzecheck[[lan]])),
    absolutePanel(
      top = 20, right = 165, style = "z-index: 1111;",
      radioGroupButtons(
        inputId = ns("settingOrHelp"), label = NULL, size = "xs",
        choices = structure(c("setting", "help"), names = c(.msg$paramset[[lan]], .msg$helpmanu[[lan]])),
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
    ),
    tabsetPanel(
      id = ns("settingOrHelp_tabs"),
      type = "hidden",
      tabPanelBody(
        "tab_setting", 
        fluidRow(
          column(
            6, 
            wellPanel(
              style = "background: white; height: 800px",
              fluidRow(
                column(
                  10, 
                  textInput(
                    inputId = ns("mulcorcol"), label = .msg$cort_var[[lan]], 
                    placeholder = .msg$cort_holder[[lan]], width = "100%")
                ),
                column(
                  2, 
                  actionButton(ns("add"), label = .msg$add[[lan]], style = "margin-top: 25px", align = "right")
                ),
                column(
                  12, 
                  multiInput(
                    inputId = ns("corcol"), label = NULL, width = "100%", choices = ""
                  )
                )
              )
            )
          ),
          column(
            6, 
            wellPanel(
              style = "background: white; height: 800px",
              tags$b(.msg$sampleinfo[[lan]]),
              DT::DTOutput(ns("phenoTab"))
            )
          )
        )
      ),
      tabPanelBody(
        "tab_help",
        wellPanel(
          style = "background: white; height: 800px", 
          img(src='inst_A02_cortest.JPG', 
              style="display: block; margin-left: auto; margin-right: auto;")
        )
      )
    )
  )
}
