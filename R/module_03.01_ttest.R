module_ttest <- function(id, esv, name_internal = "S6.1_ttest", name_show = "ttest", test = c("t.test", "DESeq2")[1]) {
  
  test <- switch(
    test, 
    "t.test" = "t.test",
    "DESeq2" = "DESeq2"
    )

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns 
      
      obj <- reactive({
        req(esv())
        list(
          pdata = getOriginDf( pData(esv()) ),
          fdata = getOriginDf( fData(esv()) ),
          expr = exprs(esv())
        )
      })
      
      ptab <- reactive({
        req(obj()$pdata)
        formatDTScrollY( obj()$pdata , height = "635px")
      })
      
      output$phenoTab <- DT::renderDT({
        req(dt <- ptab())
        if (!is.null(tsm()) && nrow(tsm()) > 0 && any(tsm()[, 1] %in% colnames(obj()$pdata))) {
          ii <- intersect(tsm()[, 1], colnames(obj()$pdata))
          dt <- formatStyle(dt, ii, backgroundColor = "#85C1E9") 
        }
        dt
      }, server = FALSE)
      
      opt <- reactive({
        req(p <- obj()$pdata)
        i <- lapply(p, function(x) {
          tb <- table(x)
          names(tb[tb > 1])
        })
        i[sapply(i, length) > 1]
      })
      
      observe({
        req(opt())
        updateSelectInput(session, inputId = "column", choices = names(opt()))
      })
      
      vs <- reactive({
        req(input$column)
        opt()[[input$column]]
      })
      
      observe( updateSelectInput(session, inputId = "grp1", choices = vs()) )
      observe( updateSelectInput(session, inputId = "grp2", choices = vs()) )
      
      df0 <- data.frame(
        "Phenotype" = character(0), 
        "Group.1" = character(0), 
        "Group.2" = character(0),
        stringsAsFactors = FALSE
      )
      
      tsm <- reactiveVal(df0)
      
      observe({
        req(at <- attr(esv(), name_internal))
        tsm( at )
      })
      
      observe({
        req(tsm())
        req(nrow(tsm()) > 0)
        req(opt())
        i0 <- apply(tsm(), 1, function(i) {
          any(!i[-1] %in% opt()[[i[1]]])
        })
        if (any(i0))
          tsm( tsm()[!i0, , drop = FALSE] )
      })
      
      observeEvent(input$clearAll, {
        tsm(df0)
      })
      
      observeEvent(input$clearSelected, {
        req(i <- input$tabTsm_rows_selected)
        tsm( tsm()[-i, , drop = FALSE] )
      })
      
      observeEvent(input$add, {
        if (input$grp1 == input$grp2) {
          showModal(modalDialog(
            .msg$valnotsame[[lan]],
            title = .msg$somethingwrong[[lan]],
            footer = modalButton(.msg$ok[[lan]])
          ))
          return(NULL)
        }
        nt <- rbind( tsm(), data.frame(
          "Phenotype" = input$column,
          "Group.1" = input$grp1,
          "Group.2" = input$grp2,
          stringsAsFactors = FALSE
        ))
        nt <- unique(nt)
        if (!is.null(tsm()))
          if (nrow(nt) == nrow(tsm())) {
            showModal(modalDialog(
              .msg$ttestexist[[lan]],
              title = .msg$somethingwrong[[lan]],
              footer = modalButton(.msg$ok[[lan]])
            ))
            return(NULL)
          }
        tsm( nt )
      })
      
      observeEvent(input$allPairs, {
        req(input$column)
        cn <- combn(unique(obj()$pdata[, input$column]), m = 2)
        nn <- data.frame(
          "Phenotype" = input$column,
          "Group 1" = cn[1, ],
          "Group 2" = cn[2, ],
          stringsAsFactors = FALSE
        )
        tsm( unique(rbind( tsm(), nn) ))
      })
      
      output$tabTsm <- DT::renderDataTable({
        tsm()
        req(tab <- tsm())
        if (nrow(tab) > 0)
          rownames(tab) <- 1:nrow(tab)
        dt <- DT::datatable( 
          tab,
          extensions = 'Scroller',
          rownames = TRUE,
          filter = "top",
          class="table-bordered compact nowrap",
          options = list(
            scrollX = TRUE, scrollY = 468, dom = 't', ordering = F, scroller = TRUE, pageLength = nrow(tab)
          )
        )
        DT::formatStyle(dt, columns = 1:ncol(tsm()), fontSize = '90%')
      })
      
      observeEvent(input$settingOrHelp, {
        updateTabsetPanel(session, "settingOrHelp_tabs", selected = paste0("tab_", input$settingOrHelp))
      })
      
      eventReactive(input$save, {
        
        if (nrow(tsm()) == 0)
          return(NULL)
        
        if (nrow(tsm()) > 32) {
          showModal(modalDialog(
            .msg$ttestmax[[lan]],
            title = .msg$somethingwrong[[lan]],
            footer = modalButton(.msg$ok[[lan]])
          ))
          return(NULL)
        }
        
        # otherwise calculate
        show_modal_spinner(text = .msg$calculating[[lan]])
        rl <- esv()
        # if parameter doesn't change, don't do anything
        if (!identical(tsm(), attr(esv(), name_internal))) {
          ofd <- fData(rl)
          gs <- attr(ofd, "GS")

          if (test == "t.test") {
            dd <- prepOmicsViewer(
              expr = obj()$expr, pData = obj()$pdata, fData = obj()$fdata, PCA = FALSE, t.test = as.matrix(tsm()), 
              ttest.fillNA = TRUE, SummarizedExperiment = FALSE)
            fd <- fData(dd)
          } else if (test == "DESeq2") {
            fd <-  prepDESeq2(
              expr = obj()$expr, pData = obj()$pdata, fData = obj()$fdata,
              invlog10 = TRUE, 
              contrast = as.matrix(tsm())
              )
          } else 
            stop ("Unknown test!")

          for (i in colnames(fd))
            ofd[, i] <- fd[, i]
          attr(ofd, "GS") <- gs
          fData(rl) <- ofd
          attr(rl, name_internal) <- tsm()
        }
        cp <- paste(tsm()[1, 2], tsm()[1, 3], sep = "_vs_")

        if (test == "t.test") {
          attr(rl, "fx") <- paste(name_show, cp, "mean.diff", sep = "|")
          attr(rl, "fy") <- paste(name_show, cp, "log.fdr", sep = "|")
        } else  {
          attr(rl, "fx") <- paste(name_show, cp, "log2FoldChange", sep = "|")
          attr(rl, "fy") <- paste(name_show, cp, "log.padj", sep = "|")
        }        
        saveRDS(rl, file = attr(rl, "filePath"))
        remove_modal_spinner()
        rl
      })
    }
  )
}

module_ttest_ui <- function(id) {
  ns <- NS(id)
  tagList(
    absolutePanel(top = -10, left = 275, tags$h2(.msg$ttesttitle[[lan]]), style = "z-index: 1111;"),
    absolutePanel(top = 5, right = 20, actionButton(inputId = ns("save"), label = .msg$analyzecheck[[lan]]), style = "z-index: 1111;" ),
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
                column(8, selectInput(inputId = ns("column"), label = .msg$ttest_selectvar[[lan]], choices = NULL, multiple = FALSE, selectize = TRUE)),
                column(4, actionButton(ns("allPairs"), label = .msg$ttest_addall[[lan]], width = "100%"), align = "right", style='padding-top:23px'),
                column(5, selectInput(inputId = ns("grp1"), label = .msg$ttestvar1[[lan]], choices = NULL, multiple = FALSE, selectize = TRUE)),
                column(5, selectInput(inputId = ns("grp2"), label = .msg$ttestvar2[[lan]], choices = NULL, multiple = FALSE, selectize = TRUE)),
                column(2, style='padding-top:23px', align = "right", actionButton(inputId = ns("add"), label = .msg$add[[lan]], width = "100%")),
                column(12, tags$b(.msg$ttestadded[[lan]]), style = "padding-bottom:8px"),
                column(12, DT::dataTableOutput(ns("tabTsm")), style = "padding-bottom:8px"),
                column(6, align = "left", actionBttn(ns("clearAll"), .msg$ttest_deleteall[[lan]], style = "minimal", size = "xs", color = "primary")),
                column(6, align = "right", actionBttn(ns("clearSelected"), .msg$ttest_deleteselected[[lan]], style = "minimal", size = "xs", color = "primary"))
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
          style = "background: white; height: 800px", # h1("这里是帮助文档")
          img(src='inst_A01_ttest.JPG', # height="750px", 
              style="display: block; margin-left: auto; margin-right: auto;")
        )
      )
    )
  )
}
