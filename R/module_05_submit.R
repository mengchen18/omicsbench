module_submit <- function(id, obj) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      conf <- reactiveValues(pca = TRUE)
      
      observe({
        at <- attr(obj(), "S5_misc")
        if (!is.null(at)) 
          for (i in names(at)) conf[[i]] <- at[[i]]
      })
      
      output$fdataTab <- DT::renderDT({
        req( obj()$fdata )
        req(at <- attr(obj(), "S2_filterRow"))
        formatDTScrollY( obj()$fdata[at$index, , drop = FALSE], height = 600 )
      })
      
      observe({
        req(fd <- obj()$fdata)
        ss <- make.names("Majority protein IDs")
        if (!is.null(conf$stringDbColumn))
          ss <- conf$stringDbColumn
        if (!ss %in% colnames(fd))
          ss <- "none"
        
        if ( !is.null( conf$seqWindowColumn ) )
          s2 <- conf$seqWindowColumn else {
            s2 <- grep("sequence.window",  colnames(fd), value = TRUE, ignore.case = TRUE)
            if (length(s2) == 0 )
              s2 <- NULL #"none"
          }
        updateMultiInput(session, inputId = "fdata_cols", choices = c("none", colnames(fd)), selected = ss)
        updateMultiInput(session, inputId = "ptm_cols", choices = colnames(fd), selected = s2)
        updateCheckboxInput(session, "pca", value = conf$pca)
      })
      
      observeEvent(input$settingOrHelp, {
        updateTabsetPanel(session, "settingOrHelp_tabs", selected = paste0("tab_", input$settingOrHelp))
      })
      
      ###########################################
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
        at <- list(
          stringDbColumn = input$fdata_cols, 
          seqWindowColumn = input$ptm_cols,
          pca = input$pca
        )
        attr(rl, "S5_misc") <- at
        saveRDS(rl, file = ff)
        rl
      })
    }
  )
}

module_submit_ui <- function(id) {
  ns <- NS(id)
  tagList(
    absolutePanel(top = -10, left = 275, tags$h2(.msg$other_title[[lan]]), style = "z-index: 1111;"),
    absolutePanel(top = 5, right = 20, actionButton(inputId = ns("save"), label = .msg$other_finish[[lan]]), style = "z-index: 1111;" ),
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
          column(
            6, 
            wellPanel(
              style = "background: white; height: 800px",
              tags$h3(.msg$featureinfo[[lan]]),
              DT::dataTableOutput(ns("fdataTab"))
            )
          ),
          column(
            6, 
            wellPanel(
              style = "background: white; height: 800px",
              tags$h3(.msg$other_set[[lan]]),
              selectInput(
                ns("fdata_cols"), label = .msg$other_string[[lan]], 
                choices = NULL, multiple = FALSE),
              selectInput(ns("ptm_cols"), label = .msg$other_seqlogo[[lan]], choices = NULL, multiple = TRUE),
              checkboxInput(ns("pca"), label = .msg$other_pca[[lan]], value = TRUE)
            )
          )
        )
      ),
      tabPanelBody(
        "tab_help",
        wellPanel(
          style = "background: white; height: 800px", # h1("这里是帮助文档")
          img(src='inst_05_misc.jpg', # height="750px", 
              style="display: block; margin-left: auto; margin-right: auto;")
        )
      )
    )
  )
}

# 
# # # # 
# library(shiny)
# a <- readRDS("~/projects/SXLj_GtNH9g0yKFCq5f2wY3NqCrBVhpR5uQB/P03/obj.RDS")
# attr(a, "S3_normalization")
# 
# ui <- fluidPage(
#   module_submit_ui("test")
# )
# 
# server <- function(input, output, session) {
#   qq <- module_submit("test", obj = reactive(a))
#   observe({
#     print( attr(qq(), "S5_misc") )
#   })
# }
# 
# shinyApp(ui, server)
