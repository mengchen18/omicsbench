module_filterRow <- function(id, obj) { 
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      conf <- reactiveValues()
      
      observeEvent(obj(), {
        req(at <- attr(obj(), "S2_filterRow"))
        conf$rowMaxMinQuantile <- at$rowMaxMinQuantile
        conf$groupMinVar <- at$groupMinVar
        conf$groupMinVarN <- at$groupMinVarN
        conf$index <- at$index
        if (!is.null(at$rowMaxMinQuantile)) {
          updateCheckboxInput(session, inputId = "rowMax", value = TRUE)
          updateTextInput(session, inputId = "rowMaxPercent", value = at$rowMaxMinQuantile)
        }
        if (!is.null(at$groupMinVarN)) 
          updateCheckboxInput(session, inputId = "phenoVar", value = TRUE)
      })
      
      # excluding samples according to obj
      dat <- reactive({
        req(d <- obj())
        at <- attr(d, "S1_dataLoading")
        i <- !at$label %in% at$sampleExclusion
        d$exprs <- d$exprs[, i, drop = FALSE]
        d$pdata <- d$pdata[i, , drop = FALSE]
        d
      })
      
      # update accordingly
      featureTab_include <- reactiveVal()
      featureTab_exclude <- reactiveVal()
      rv <- reactiveVal()
      
      observeEvent(dat(), {
        req( dat()$pdata )
        t0 <- sapply( dat()$pdata, function(x) max(table(x)) )
        t0 <- t0[t0 > 1]
        rv(t0)
        updateSelectInput( session, inputId = "phenoCol", choices = names(t0), selected = conf$groupMinVar )
        
      })
      
      observeEvent( input$phenoCol, {
        req(input$phenoCol)
        req(rv())
        v <- 2
        if (!is.null(conf$groupMinVarN))
          v <- conf$groupMinVarN
        updateSelectInput(session, inputId = "atLeastN", choices = 1:rv()[input$phenoCol], selected = v)
      })
      
      observeEvent(input$save, {
        if (!input$rowMax) {
          conf$rowMaxMinQuantile <- NULL
        } else
          conf$rowMaxMinQuantile <- c2n(input$rowMaxPercent)
      })
      
      observeEvent(input$save, {
        if (!input$phenoVar) {
          conf$groupMinVar <- NULL
          conf$groupMinVarN <- NULL
        } else {
          conf$groupMinVar <- input$phenoCol
          conf$groupMinVarN <- input$atLeastN
        }
      })
      
      observeEvent(input$save, {
        if ( !input$rowMax && !input$phenoVar ) {
          conf$index <- rep(TRUE, nrow(dat()$expr))
          return()
        }
        vv <- conf$rowMaxMinQuantile
        if (is.null(vv))
          vv <- -Inf
        if (vv > 0 && vv < 1)
          vv <- quantile(dat()$exprs, na.rm = TRUE, probs = vv)
        conf$index <- filterRow(
          dat()$exprs, max.value = vv, var = conf$groupMinVar, min.rep = as.integer(conf$groupMinVarN)
        )
      })
      
      observe({
        req(dat()$fdata)
        v <- conf$index
        if (is.null(v))
          v <- rep(TRUE, nrow(dat()$fdata))
        featureTab_include( dat()$fdata[which(v), , drop = FALSE] )
        featureTab_exclude( dat()$fdata[which(!v), , drop = FALSE] )
      })
      
      output$phenoTab <- DT::renderDT({
        req(tab <- dat()$pdata)
        tab <- formatDTScrollY( tab, height = 475 )
        if ( input$phenoCol %in% colnames(dat()$pdata) && any(input$phenoVar) ) 
          tab <- formatStyle(tab, input$phenoCol, backgroundColor = "#85C1E9")
        tab
      }, server = FALSE)
      
      output$fTab_include <- DT::renderDT({
        req(tab <- dat()$pdata)
        formatDT( featureTab_include(), pageLength = 10)
      })
      
      output$fTab_exclude <- DT::renderDT({
        req(tab <- dat()$pdata)
        formatDT( featureTab_exclude(), pageLength = 9)
      })
      
      observeEvent(input$settingOrHelp, {
        updateTabsetPanel(session, "settingOrHelp_tabs", selected = paste0("tab_", input$settingOrHelp))
      })
      
      ###################
      observeEvent(input$save, {
        req(ff <- attr(obj(), "S1_dataLoading")$filePath)
        if ( file.exists( file.path( dirname(ff), "ESVObj.RDS" )) ) {
          showModal(
            modalDialog(
              .msg$deleteConfirm[[lan]],
              footer = actionButton(ns("deleteOld"), label = .msg$delandgo[[lan]]), easyClose = TRUE
            )
          )
        }
      })
      
      observeEvent(input$deleteOld, {
        req(ff <- attr(obj(), "S1_dataLoading")$filePath)
        er <- file.path( dirname(ff), "ESVObj.RDS" )
        unlink(er)
        shinyjs::click("save")
        removeModal()
      })
      
      eventReactive(input$save, {
        rl <- obj()
        req(ff <- attr(rl, "S1_dataLoading")$filePath)
        req(!file.exists(file.path( dirname(ff), "ESVObj.RDS" )))
        at <- reactiveValuesToList(conf)
        attr(rl, "S2_filterRow") <- at
        saveRDS(rl, file = ff)
        rl
      })
    })
}

module_filterRow_ui <- function(id) { 
  ns <- NS(id)
  tagList(
    absolutePanel(top = -10, left = 275, tags$h2(.msg$filterfeat[[lan]]), style = "z-index: 1111;"),
    absolutePanel(top = 5, right = 20, actionButton(inputId = ns("save"), label = .msg$saveset[[lan]]), style = "z-index: 1111;" ),
    absolutePanel(
      top = 20, right = 120, style = "z-index: 1111;",
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
          column(6, wellPanel(
            style = "height: 245px; background: white",
            checkboxInput(ns("rowMax"), label = .msg$filter1[[lan]]),
            conditionalPanel(
              "input.rowMax == true", ns = ns,
              textInputIcon(
                ns("rowMaxPercent"), label = NULL, icon = list(.msg$fl1[[lan]]),
                placeholder = .msg$fl1desc[[lan]])
            ),
            checkboxInput(ns("phenoVar"), label = .msg$filter2[[lan]]),
            conditionalPanel(
              "input.phenoVar == true", ns = ns,
              fluidRow(
                column(5, selectInput(ns("phenoCol"), label = .msg$choosevar[[lan]], choices = NULL)),
                column(7, selectInput(ns("atLeastN"), label = .msg$fvardesc[[lan]], choices = NULL))
              )
            )
          ),
          wellPanel(
            style = "background: white",
            tags$b(.msg$sampleinfo[[lan]]),
            DT::DTOutput(ns("phenoTab"))
          )),
          column(6, wellPanel(
            style = "background: white",
            tags$b(.msg$keepprot[[lan]]),
            DT::DTOutput(ns("fTab_include")),
            tags$hr(),
            tags$b(.msg$filterprot[[lan]]),
            DT::DTOutput(ns("fTab_exclude"))
          ))
        )
      ),
      tabPanelBody(
        "tab_help",
        wellPanel(
          style = "background: white; height: 800px", 
          img(src='inst_02_filter.JPG', # height="750px", 
              style="display: block; margin-left: auto; margin-right: auto;")
        )
      )
    )
  )
}

# library(shiny)
# source("package/R/shiny_auxi.R")
# 
# a <- readRDS("~/projects/SXLj_GtNH9g0yKFCq5f2wY3NqCrBVhpR5uQB/P01/obj.RDS")
# # filterRow(a$exprs, max.value = quantile(a$exprs, na.rm = TRUE, probs = 0.1), var = NULL, min.rep = 2)
# 
# ui <- fluidPage(
#   module_filterRow_ui("filterrow")
# )
# 
# server <- function(input, output, session) {
#   qq <- module_filterRow("filterrow", obj = reactive(a))
#   observe({
#     print( names(qq()) )
#   })
# }
# 
# shinyApp(ui, server)

