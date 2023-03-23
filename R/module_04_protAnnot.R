module_annot <- function(id, obj, annot_dir_global) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      conf <- reactiveValues()
      
      observe({
        req(at <- attr(obj(), "S4_annotation"))
        for (i in names(at))
          conf[[i]] <- at[[i]]
      })
      
      fdata <- reactive({
        req(fd <- obj()$fdata)
        req(at <- attr(obj(), "S2_filterRow"))
        fd[at$index, , drop = FALSE]
      })
      
      observe({
        req(fd <- fdata())
        ss <- ""
        if (!is.null(conf$fdataColumn))
          ss <- conf$fdataColumn else if (make.names("Majority protein IDs") %in% colnames(fd))
            ss <- make.names("Majority protein IDs")
        updateSelectInput(session, inputId = "fdata_cols", choices = c("", colnames(fd)), selected = ss)
      })
      
      annot_dir <- reactive({
        ss <- NULL
        ss0 <- NULL
        if (!is.null(attr(obj(), "S1_dataLoading"))) {
          d0 <- dirname(attr(obj(), "S1_dataLoading")$filePath)
          ss <- file.path(d0, ".gsannot")
          ss0 <- file.path(dirname(d0), ".gsannot")
        }
        c(annot_dir_global, ss0, ss)
      })
      
      observe({
        af <- lfname(annot_dir())
        v <- NULL
        if (!is.null(conf$annotationFile))
          v <- conf$annotationFile
        updateSelectInput(session, inputId = "annotdb", choices = af, selected = v)
      })
      
      observe({
        if (is.null(conf$annotationFile))
          return(NULL)
        updateSelectInput(session, inputId = "annotdb", selected = conf$annotationFile)
      })
      
      observe({
        if (is.null(conf$isoformSeparator) || !conf$isoformSeparator %in% c("dot (.)", "dash (-)"))
          return(NULL)
        updateSelectInput(session, inputId = "isosep", selected = conf$isoformSeparator)
      })
      
      output$fdata_tab <- DT::renderDT({
        req(fdata())
        dt <- formatDTScrollY( fdata()[1:min(100, nrow(fdata)), , drop = FALSE], height = 525)
        if (input$fdata_cols %in% colnames(fdata()))
          dt <- formatStyle(dt, input$fdata_cols, backgroundColor = "#85C1E9") 
        dt
      }, server = FALSE)
      
      annotfile <- reactive({
        if (is.null(input$annotdb))
          return(NULL)
        if (input$annotdb == "")
          return(NULL)
        if (!grepl(".annot$", input$annotdb))
          return(NULL)
        input$annotdb
      })
      
      annotFile <- reactive({
        req(annotfile())
        ret <- read.delim(annotfile(), stringsAsFactors = FALSE, nrow = 100)
        i <- colnames(ret) == "source"
        if (!any(i)) return(ret)
        
        cls <- rep("NULL", ncol(ret))
        cls[i] <- "character"
        tab <- read.delim(annotfile(), colClasses = cls)
        stats <- table(tab$source)
        lab <- names(stats)
        names(lab) <- paste0(names(stats), " (", stats, ")")
        attr(ret, "source") <- lab
        ret
      })
      
      annotMatchCol <- reactiveVal()
      observe({
        req(annotFile())
        ss <- colnames(annotFile())
        s1 <- "ACC"
        if (!is.null(conf$annotationColumn) && nchar(conf$annotationColumn) > 0)
          s1 <- conf$annotationColumn
        updateSelectInput(session = session, inputId = "amatchcol" , choices = ss, selected = intersect(s1, ss))
      })
      observe({
        annotMatchCol(input$amatchcol)
      })
      observe({
        req(tab <- annotFile())
        lab <- attr(tab, "source")
        if (!is.null(conf$annotationFile)) {
          default <- conf$annotationSource
        } else {
          default <- intersect(lab, c("InterPro", "UniPathway", "KEGG", "GO", "ComplexPortal"))
          if (length(default) == 0)
            default <- lab
        }
        updateMultiInput(session, inputId = "adbs", choices = lab, selected = default)
      })
      
      output$annotdb_tab <- DT::renderDT({
        annotFile()
        req(annotFile())
        dt <- formatDTScrollY(annotFile(), height = 225)
        if (!is.null(annotMatchCol()) && nchar(annotMatchCol()) > 0)
          dt <- formatStyle(dt, annotMatchCol(), backgroundColor = "#85C1E9")
        dt
      }, server = FALSE)
      
      observeEvent(input$settingOrHelp, {
        updateTabsetPanel(session, "settingOrHelp_tabs", selected = paste0("tab_", input$settingOrHelp))
      })
      
      ######################
      observeEvent(input$save, {
        req(ff <- attr(obj(), "S1_dataLoading")$filePath)
        if ( file.exists( file.path( dirname(ff), "ESVObj.RDS" )) ) {
          showModal(
            modalDialog(
              .msg$deleteConfirm[[lan]],
              footer = actionButton(ns("deleteOld"), label = "删除并进入下一步"), easyClose = TRUE
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
          annotationFile = input$annotdb, #file.path(annot_dir(), input$annotdb),
          annotationColumn = input$amatchcol,
          fdataColumn = input$fdata_cols,
          isoformSeparator = input$isosep,
          annotationSource = input$adbs
        )
        attr(rl, "S4_annotation") <- at
        saveRDS(rl, file = ff)
        rl
      })
    }
  )
}

module_annot_ui <- function(id) { #}, annot_dir) {
  
  ns <- NS(id)
  tagList(
    absolutePanel(top = -10, left = 275, tags$h2(.msg$annot_title[[lan]]), style = "z-index: 1111;"),
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
          column(
            6, 
            wellPanel(
              style = "background: white; height: 800px",
              tags$h3(.msg$annot_database[[lan]]),
              fluidRow(
                column(
                  8, selectInput(ns("annotdb"), label = .msg$annot_databaseselect[[lan]], choices = NULL, multiple = FALSE, selected = "")
                ),
                column(
                  4, selectInput(ns("amatchcol"), label = .msg$annot_databasecol[[lan]], choices = "", multiple = FALSE, selected = "")
                )
              ),
              tags$b(.msg$annot_show[[lan]]),
              DT::dataTableOutput(ns("annotdb_tab")),
              multiInput(inputId = ns("adbs"), label = .msg$annot_source[[lan]], choices = ""),
            )
          ),
          column(
            6, 
            wellPanel(
              style = "background: white; height: 800px",
              tags$h3(.msg$featureinfo[[lan]]),
              fluidRow(
                column(
                  8, selectInput(ns("fdata_cols"), label = .msg$annot_datacol[[lan]], choices = NULL, multiple = FALSE)
                ),
                column(
                  4, selectInput(ns("isosep"), label = .msg$annot_subsep[[lan]], choices = c("none"="none", "point (.)"="dot (.)", "dash (-)"="dash (-)"))
                )),
              DT::dataTableOutput(ns("fdata_tab"))
            )
          )
        )
      ),
      tabPanelBody(
        "tab_help",
        wellPanel(
          style = "background: white; height: 800px", # h1("这里是帮助文档")
          img(src='inst_04_annot.JPG', # height="750px", 
              style="display: block; margin-left: auto; margin-right: auto;")
        )
      )
    )
  )
}

# # # 
# library(shiny)
# a <- readRDS("~/projects/SXLj_GtNH9g0yKFCq5f2wY3NqCrBVhpR5uQB/P03/obj.RDS")
# attr(a, "S3_normalization")
# 
# ui <- fluidPage(
#   module_annot_ui("test", annot_dir = "/mnt/NAS1/annotDb/")
# )
# 
# server <- function(input, output, session) {
#   qq <- module_annot("test", obj = reactive(a), annot_dir = "/mnt/NAS1/annotDb/")
#   observe({
#     print( attr(qq(), "S4_annotation") )
#   })
# }
# 
# shinyApp(ui, server)
