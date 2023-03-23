#' importFrom shinycssloaders withSpinner
module_normalization <- function(id, obj) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      conf <- reactiveValues()
      
      observe({
        at <- attr(obj(), "S3_normalization")
        if (is.null(at)) {
          conf$rowwise <- "None"
          conf$columnwise <- "None"
        } else {
          conf$rowwise <- at$rowwise
          conf$columnwise <- at$columnwise
        }
      })
      
      # ============================== data input ==============================
      dat <- reactive({
        req(obj <- obj())
        req(attr(obj, "S2_filterRow"))
        expr <- obj$exprs
        pd <- obj$pdata
        at1 <- attr(obj, "S1_dataLoading")
        at2 <- attr(obj, "S2_filterRow")
        ic <- which(!at1$label %in% at1$sampleExclusion)
        ir <- which(at2$index)
        list(
          exprs = expr[ir, ic, drop = FALSE],
          pdata = pd[ic, , drop = FALSE],
          fdata = obj$fdata[ir, , drop = FALSE]
        )
      })
      
      # normalize exprs
      observe({
        req(pd <- dat()$pdata)
        updateSelectInput(session, inputId = "selectPheno", choices = c("None", colnames(pd)))
        updateSelectInput(session, inputId = "tooltip", choices = colnames(pd))
        updateSelectInput(session, inputId = "normCol", selected = conf$columnwise)
        cc <- c("None" = "None")
        if (refcol <- "Reference" %in% colnames(pd)) {
          if (sum(pd$Reference) > 2)
            cc <- c(cc, "Reference"="Reference")
        }
        if ("Batch" %in% colnames(pd)) {
          if ( length(unique(pd$Batch)) >= 2 ) {
            cc <- c(cc, "Batch mean" = "Batch mean")
            if (refcol) {
              if (all(sapply(split(pd$Reference, pd$Batch), any)))
                cc <- c(cc, "Batch reference" = "Batch reference")
            }
          }
        }
        cc <- unname(cc)
        updateAwesomeRadio(session, inputId = "normRow",  choices = cc, selected = conf$rowwise)
      })
      # 
      # 
      datNorm <- eventReactive(input$test, {
        d0 <- normalizeData(
          dat()$exprs, colWise = input$normCol, rowWise = input$normRow,
          ref = dat()$pdata$Reference, batch = dat()$pdata$Batch
        )
        i <- which(rowSums(!is.na(d0)) > 0)
        list(
          exprs = d0[i, , drop = FALSE],
          fdata = dat()$fdata[i, , drop = FALSE]
        )
      })
      
      output$download <- downloadHandler(
        filename = function() {
          paste("proteinGroups", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          writeTriplet(expr = datNorm()$exprs, pd = dat()$pdata, fd = datNorm()$fdata, file = file, creator = "BayBioMS")
        }
      )
      
      # render feature data
      output$fdataTab <- DT::renderDT({
        req(datNorm()$fdata)
        formatDTScrollY( datNorm()$fdata, height = 470)
      })
      ## ====================== calculation ========================
      stats <- eventReactive(input$test, {
        req(expr <- datNorm()$exprs)
        show_modal_spinner(text = .msg$calculating[[lan]])
        r <- getQCStats( expr )
        remove_modal_spinner()
        r
      })
      
      ###### barplot ######
      output$barplot <- renderPlotly({
        req(input$test > 0)
        req(pd <- dat()$pdata)
        cutnumorchar <- function(x, n = 60, alt = "") {
          if (is.character(x) || is.factor(x)) {
            message (.msg$toomanycolor[[lan]])
            v <- alt
          } else if (is.numeric(x)) {
            v <- as.character(cut(x, breaks = n, include.lowest = TRUE, dig.lab = 3))
          } else
            stop("cutnumorchar: x needs to be one of objects: numeric, character, factor")
          v
        }
        req(nrow(pd) == length(stats()$nval)) # not ideal solution
        req(input$selectPheno)
        data <- data.frame(
          x = names(stats()$nval),
          y = stats()$nval,
          dec = stats()$nvalInt,
          inc = stats()$nvalCum,
          col = "# ID",
          stringsAsFactors = FALSE
        )
        data$x <- factor(data$x, levels = unique(data$x))
        cc <- "gray"
        if (input$selectPheno %in% colnames(pd)) {
          data$col <- pd[, input$selectPheno]
          cc <- omicsViewer:::nColors(k = length(unique(data$col)))
          if (length(unique(data$col)) > 60) data$col  <- cutnumorchar(data$col, alt = "# ID")
        }
        if (is.numeric(data$col)) data$col <- as.character(data$col)
        fig <- plot_ly( data )
        fig <- add_trace(fig, x = ~ x, y = ~ y, type = 'bar', color = ~ col, colors = cc )
        if (!is.null(data$dec))
          fig <- add_trace(fig, x = ~ x, y = ~ dec, type = 'scatter', mode = 'lines+markers', name = 'Shared')
        if (!is.null(data$inc))
          fig <- add_trace(fig, x = ~ x, y = ~ inc, type = 'scatter', mode = 'lines+markers', name = 'Cummu.')
        fig <- layout(fig, xaxis = list(title = ""), yaxis = list(title = "ID"), legend = list(
          orientation = 'h', x = 0, y = 1.01, yanchor='bottom')
        )
        toWebGL(fig)
      })
      
      observe({
        req(stats()$pcImp)
        cn <- colnames(stats()$pcImp)
        names(cn) <- sub("^\\|", "", cn)
        updateSelectInput(session, inputId = "pcx", choices = cn, selected = cn[1])
        updateSelectInput(session, inputId = "pcy", choices = cn, selected = cn[2])
      })
      
      callModule(
        omicsViewer:::plotly_boxplot_module, id = "boxplotly",
        reactive_param_plotly_boxplot = reactive({
          req( datNorm()$exprs )
          req(input$test > 0)
          list( x = datNorm()$exprs, i = input$fdataTab_rows_selected, ylab = "Intensity" )
        }),
        reactive_checkpoint = reactive(TRUE)
      )
      
      pcax <- reactive({
        c(match(input$pcx, colnames(stats()$pcImp)), match(input$pcy, colnames(stats()$pcImp)))
      })
      # 
      prepPCScatter <- function(r) {
        req(input$pcx)
        req(input$pcy)
        l <- list()
        l$x <- r[, pcax()[1]]
        l$y <- r[, pcax()[2]]
        l$xlab <- sub("^\\|", "", colnames(r)[pcax()[1]])
        l$ylab <- sub("^\\|", "", colnames(r)[pcax()[2]])
        if (input$selectPheno %in% colnames(dat()$pdata))
          l$color <- dat()$pdata[, input$selectPheno]
        l$tooltips <- dat()$pdata[, input$tooltip]
        l
      }
      #     
      #     # plot no imputation
      stats_pca_noimpute <- reactive({
        req(r <- stats()$pcNoImp)
        req( dat() )
        req(input$test > 0)
        prepPCScatter(r)
      })
      
      v_scatter_noimpute <- callModule(
        omicsViewer:::plotly_scatter_module, id = "pca_noimpute", reactive_param_plotly_scatter = stats_pca_noimpute
      )
      #     
      #     # plot imputed
      stats_pca_impute <- reactive({
        req(dat())
        req(r <- stats()$pcImp)
        req(input$test > 0)
        prepPCScatter(r)
      })
      
      v_scatter_impute <- callModule(
        omicsViewer:::plotly_scatter_module, id = "pca_impute", reactive_param_plotly_scatter = stats_pca_impute
      )
      
      observeEvent(input$settingOrHelp, {
        updateTabsetPanel(session, "settingOrHelp_tabs", selected = paste0("tab_", input$settingOrHelp))
      })
      ####
      
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
        if (input$test == 0) {
          showModal(modalDialog(
            .msg$checknorm[[lan]],
            title = .msg$onemorecheck[[lan]],
            footer = modalButton(.msg$ok[[lan]]),
            size = c("m", "s", "l")[1]
          ))
          return(NULL)
        }
        at <- list(
          rowwise = input$normRow,
          columnwise = input$normCol
        )
        attr(rl, "S3_normalization") <- at
        saveRDS(rl, file = ff)
        rl
      })
    }
  )
}

module_normalization_ui <- function(id) {
  ns <- NS(id)
  tagList(
    absolutePanel(top = -10, left = 275, tags$h2(.msg$datanorm[[lan]]), style = "z-index: 1111;"),
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
        sidebarLayout(
          sidebarPanel(
            style = "height: 973px; background-color:white",
            awesomeRadio(
              inputId = ns("normCol"),
              label = .msg$norm_col[[lan]], 
              # choices = c("不使用" = "None", 
              #             "中位数对齐" = "Median centering", 
              #             "中位数对齐（所有样本中检测到的蛋白）" = "Median centering (shared ID)", 
              #             "表达强度总和对齐" = "Total sum", 
              #             "中位数对齐+方差归一" = "median centering + variance stablization"),
              choices = c("None", 
                          "Median centering", 
                          "Median centering (shared ID)", 
                          "Total sum", 
                          "median centering + variance stablization"),
              inline = FALSE
            ),
            awesomeRadio(
              inputId = ns("normRow"),
              label = .msg$norm_row[[lan]], 
              choices = "None",
              selected = "None",
              inline = FALSE
            ),
            actionButton(inputId = ns("test"), label = .msg$checkres[[lan]]),
            downloadButton(outputId = ns("download"), label = .msg$norm_export[[lan]]),
            tags$hr(),
            tags$b(.msg$norm_checkrow[[lan]]),
            DT::DTOutput(ns("fdataTab"))
          ), 
          mainPanel(
            
            wellPanel(
              style = "background: white;",
              conditionalPanel(
                "input.test != 0", ns = ns,
              fluidRow(
                column(3, selectInput(ns("selectPheno"), label = .msg$norm_colorvar[[lan]], choices = NULL, selected = NULL, multiple = FALSE, selectize = TRUE)),
                column(3, selectInput(ns("tooltip"), label = .msg$norm_sampleinfo[[lan]], choices = NULL, selected = NULL, multiple = FALSE, selectize = TRUE)),
                column(3, selectInput(ns("pcx"), label = .msg$norm_comp1[[lan]], choices = NULL, selected = NULL, multiple = FALSE, selectize = TRUE)),
                column(3, selectInput(ns("pcy"), label = .msg$norm_comp2[[lan]], choices = NULL, selected = NULL, multiple = FALSE, selectize = TRUE)),
                column(
                  width = 6, 
                  tags$b(.msg$norm_l1[[lan]]),
                  omicsViewer:::plotly_scatter_ui(ns("pca_noimpute"), height = "366px")
                ),
                column(
                  width = 6,
                  tags$b(.msg$norm_l2[[lan]]),
                  omicsViewer:::plotly_scatter_ui(ns("pca_impute"), height = "366px")
                ),
                column(
                  width = 6, 
                  tags$b(.msg$norm_l3[[lan]]),
                  omicsViewer:::plotly_boxplot_ui(ns("boxplotly"))
                ),
                column(
                  width = 6, 
                  tags$b(.msg$norm_l4[[lan]]),
                  shinycssloaders::withSpinner(
                    plotlyOutput(ns("barplot")), type = 8, color = "green"
                  )
                  )
                )
              )
            )
          )
        )
      ),
      tabPanelBody(
        "tab_help",
        wellPanel(
          style = "background: white; height: 800px", # h1("这里是帮助文档")
          img(src='inst_03_norm.JPG', # height="750px", 
              style="display: block; margin-left: auto; margin-right: auto;")
        )
      )
    )
  )
}

