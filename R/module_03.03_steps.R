module_steps <- function(id, esv) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns 
      
      ptab <- reactive({
        req(esv())
        getOriginDf( pData(esv()) )
      })
      
      output$phenoTab <- DT::renderDT({
        req(ptab())
        dt <- formatDTScrollY( ptab(), height = "380px" )
        if (!is.null(tsm()) && nrow(tsm()) > 0 && any(tsm()[, 1] %in% colnames(ptab()))) {
          ii <- intersect(tsm()[, 1], colnames(ptab()))
          dt <- formatStyle(dt, ii, backgroundColor = "#85C1E9") 
        }
        dt
      }, server = FALSE)
      
      opt <- reactive({
        req(p <- ptab())
        i <- lapply(p, function(x) {
          tb <- table(x)
          names(tb[tb > 1])
        })
        i[sapply(i, length) > 2]
      })
      
      observe({
        req(opt())
        updateSelectInput(session, inputId = "column", choices = names(opt()))
      })
      
      vs <- reactive({
        req(input$column)
        opt()[[input$column]]
      })
      
      output$bucketOutput <- renderUI({
        req(length(vs()) > 2)
        bucket_list(
          header = NULL,
          orientation = "horizontal",
          options = sortable_options(style='padding:1px'),
          add_rank_list(
            text = .msg$step_allgroup[[lan]],
            labels = vs()
          ),
          add_rank_list(
            text = .msg$step_order[[lan]],
            labels = NULL,
            input_id = ns("orderedList")
          )
        )
      })
      
      tsm <- reactiveVal(
        data.frame(Phenotype = character(0), group.1 = character(0), group.2 = character(0), group.3 = character(0))
      )
      
      observe({
        req(i <- attr(esv(), "S6.4_step"))
        tsm( i )
      })
      
      observe({
        req(tsm())
        req(nrow(tsm()) > 0)
        req(opt())
        i0 <- apply(tsm(), 1, function(i) {
          any(!i[-1] %in% c("", opt()[[i[1]]]))
        })
        if (any(i0))
          tsm( tsm()[!i0, , drop = FALSE] )
      })
      
      observeEvent(input$clearAll, {
        tsm(tsm()[numeric(0), ])
      })
      
      frbind <- function(x, y) {
        if (!inherits(x, c("character", "numeric", "matrix", "data.frame")) ||
            !inherits(y, c("character", "numeric", "matrix", "data.frame")))
          stop("x and y should be one of classes - vector, matrix, data.frame")
        if (is.null(ncol(x)))
          x <- matrix(x, nrow = 1)
        if (is.null(ncol(y)))
          y <- matrix(y, nrow = 1)
        xnc <- ncol(x)
        ync <- ncol(y)
        if (xnc > ync) {
          y <- cbind(y, matrix("", nrow = nrow(y), ncol = xnc - ync))
        } else if (xnc < ync) {
          x <- cbind(x, matrix("", nrow = nrow(x), ncol = ync - xnc))
        }
        colnames(x) <- colnames(y) <- c("Phenotype", paste0("Group.", 1:(ncol(x) - 1)))
        data.frame(rbind(x, y), stringsAsFactors = FALSE)
      }
      
      observeEvent(input$add, {
        req( input$orderedList )
        
        if (length(input$orderedList) < 3) {
          showModal(modalDialog(
            title = .msg$somethingwrong[[lan]],
            .msg$step_less2[[lan]]
          ))
          return(NULL)
        }
        
        if (is.null(tsm())) {
          nt <- matrix(c(input$column, input$orderedList ), nrow = 1)
        } else {
          nt <- frbind( tsm(), c(input$column, input$orderedList ))
          nt <- unique(nt)
        }
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
      
      output$tabTsm <- renderDT({
        tsm()
        req(tab <- tsm())
        if (nrow(tab) > 0)
          rownames(tab) <- 1:nrow(tab)
        dt <- DT::datatable( 
          tab,
          selection =  "multiple",
          rownames = TRUE,
          caption = NULL,
          class="table-bordered compact nowrap",
          options = list(scrollX = TRUE, scrollY = "150px", dom = 't', ordering = FALSE, pageLength = nrow(tab))
        )
        DT::formatStyle(dt, columns = 1:ncol(tsm()), fontSize = '90%')
      })
      
      observeEvent(input$clearSelected, {
        req(i <- input$tabTsm_rows_selected)
        tsm( tsm()[-i, , drop = FALSE] )
      })
      
      getStepList <- function(x) {
        if (is.null(x))
          return(NULL)
        oa <- split(as.matrix(x), row(x))
        lapply(oa, setdiff, "")
      }
      
      observeEvent(input$settingOrHelp, {
        updateTabsetPanel(session, "settingOrHelp_tabs", selected = paste0("tab_", input$settingOrHelp))
      })
      
      eventReactive(input$save, {

        if (nrow(tsm()) == 0)
          return(NULL)

        if (nrow(tsm()) > 12) {
          showModal(modalDialog(
            .msg$step_max[[lan]],
            title = .msg$somethingwrong[[lan]],
            footer = modalButton(.msg$ok[[lan]])
          ))
          return(NULL)
        }
        
        show_modal_spinner(text = .msg$calculating[[lan]])
        rl <- esv()
        oa <- getStepList(attr(esv(), "S6.4_step"))
        na <- getStepList(tsm())
        ra <- setdiff(na, oa)
        if (length(ra) > 0) {
          sa <- stepAnalysis(expr = exprs(esv()), pdata = ptab(), steps = ra)
          ofd <- fData(rl)
          gs <- attr(ofd, "GS")
          for (i in setdiff(colnames(sa), colnames(ofd)))
            ofd[, i] <- sa[, i]
          attr(ofd, "GS") <- gs
          fData(rl) <- ofd
          attr(rl, "S6.4_step") <- tsm()
        }
        # 
        cp <- paste(na[[1]], collapse = ":")
        attr(rl, "fx") <- paste("step", cp, "x.fdr.1", sep = "|")
        attr(rl, "fy") <- paste("step", cp, "y.mean.diff", sep = "|")
        saveRDS(rl, file = attr(rl, "filePath"))
        remove_modal_spinner()
        rl
      })
    }
  )
}

module_steps_ui <- function(id) {
  ns <- NS(id)
  tagList(
    absolutePanel(top = -10, left = 275, tags$h2(.msg$step_title[[lan]]), style = "z-index: 1111;"),
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
              selectInput(inputId = ns("column"), label = .msg$ttest_selectvar[[lan]], choices = NULL, multiple = FALSE, selectize = TRUE),
              uiOutput(ns("bucketOutput")),
              column(12, actionButton(inputId = ns("add"), label = .msg$add[[lan]]), align = "right")
            )
          ),
          column(
            6, 
            wellPanel(
              style = "background: white; height: 800px",
              tags$b(.msg$sampleinfo[[lan]]),
              DT::DTOutput(ns("phenoTab")),
              br(),
              tags$b(.msg$step_added[[lan]]),
              DT::DTOutput(ns("tabTsm")),
              fluidRow(
                column(
                  6, align = "left",
                  actionBttn(ns("clearAll"), .msg$ttest_deleteall[[lan]], style = "minimal", color = "primary", size = "xs")), 
                column(
                  6, align = "right",
                  actionBttn(ns("clearSelected"), .msg$ttest_deleteselected[[lan]], style = "minimal", color = "primary", size = "xs"))
              )
            )
          )
        )
      ),
      tabPanelBody(
        "tab_help",
        wellPanel(
          style = "background: white; height: 800px", 
          img(src='inst_A03_step.JPG', # height="750px", 
              style="display: block; margin-left: auto; margin-right: auto;")
        )
      )
    )
  )
}
