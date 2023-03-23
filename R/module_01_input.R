

#' @import shinyFiles
module_input <- function(id, dir) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      dat <- reactiveVal()
      conf <- reactiveValues()
      
      observeEvent(dir(), {
        req(dir())
        cff <- file.path(dirname(dir()), "obj.RDS")
        if ( file.exists(cff) ) {
          # if exist already, load it
          dat( readRDSWithSpinner(cff) )
          conf$fdata <- dat()$fdata
          conf$pdata <- dat()$pdata
          conf$exprs <- dat()$exprs
          conf$label <- attr(dat(), "S1_dataLoading")$label
          conf$sampleExclusion <- attr(dat(), "S1_dataLoading")$sampleExclusion
        } else {
          # if not exist, pop up and define 
          tab <- read.delim(dir(), stringsAsFactors = FALSE, nrows = 100)
          ic <- colnames(tab)[sapply(tab, is.numeric)]
          showModal(modalDialog(
            fluidRow(
              column(
                6, textInput(
                  inputId = ns("mulexprcol"), label = "Select columns with pattern:", 
                  placeholder = "Using regular expression, such as '^Intensity'.", width = "100%"
                )),
              column(
                2, actionButton(ns("add"), label = "Add", style = "margin-top: 25px")
              ),
              column(
                2, actionButton(ns("deselect_all"), label = "Deselected all", style = "margin-top: 25px", align = "right")
              ),
              column(
                2, actionButton(ns("select_all"), label = "Select all", style = "margin-top: 25px", align = "right")
              )
            ),
            multiInput(ns("exprsCol"), "Columns of expression matrix", choices = ic, width = "100%"),
            title = "Select columns of expression matrix",
            footer = actionButton(ns("exprsColDone"), label = "Done"), size = "l"
          ))
        }
      })
      
      observeEvent(input$add, {
        tab <- read.delim(dir(), stringsAsFactors = FALSE, nrows = 1)
        choi <- colnames(tab)
        updateMultiInput(session, inputId = "exprsCol", selected = grep(input$mulexprcol, choi, value = TRUE))
      })
      
      observeEvent(input$select_all, {
        tab <- read.delim(dir(), stringsAsFactors = FALSE, nrows = 1)
        choi <- colnames(tab)
        updateMultiInput(session, inputId = "exprsCol", selected = choi)
      })
      
      observeEvent(input$deselect_all, {
        updateMultiInput(session, inputId = "exprsCol", selected = "")
      })
      
      observeEvent(input$exprsColDone, {
        removeModal()
        tab <- read.delim(dir(), stringsAsFactors = FALSE)
        expr <- apply(tab[, input$exprsCol, drop = FALSE], 2, log10)
        expr[is.infinite(expr)] <- NA
        rownames(expr) <- make.names(rownames(tab))
        
        conf$exprs <- expr
        conf$fdata <- tab[, setdiff(colnames(tab), input$exprsCol), drop = FALSE]
        if (ncol(conf$fdata) == 0)
          conf$fdata <- data.frame(
            rowID = sprintf("FT%06d", seq_len(nrow(expr))), 
            stringsAsFactors = FALSE)
        conf$label <- input$exprsCol
        conf$pdata <- phenoTemplate(conf$label)
      })
      
      ## ================= phenotype data components ==================
      pdata <- reactiveVal()
      observe( {
        req(pdata())
        conf$pdata <- pdata() 
      })
      
      output$tab <- renderExcel({
        req(tab <- conf$pdata)
        excelTable(
          data = tab, columns = data.frame(type = excelR:::get_col_types(tab)), 
          allowDeleteRow = FALSE, allowInsertRow = FALSE, columnSorting = FALSE,
          colHeaders = colnames(tab), tableHeight = "525px")
      })
      
      e2r <- function(x, alt) {
        if (!is.null(x))
          r <- excel_to_R(x) else
            r <- alt
          validNumber <- function(x) !grepl("\\D", gsub("\\.|NA", "", x))
          ii <- which(sapply(r, function(x) all(validNumber(x), na.rm = TRUE)))
          r[ii] <- lapply(r[ii], function(x) as.numeric(as.character(x)))
          r
      }
      
      output$saveTemplate <- downloadHandler(
        filename = function() { "phenotypeData.tsv" },
        content = function(file) {
          print(input$tab)
          write.table(e2r(input$tab, conf$pdata), file, col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")
        })
      
      observeEvent(input$uploadTemplate$datapath, {
        req(ff <- input$uploadTemplate$datapath)
        t0 <- NULL
        if (grepl(".csv$", ff)) {
          t0 <- read.csv(ff, stringsAsFactors = FALSE, header = TRUE)
        } else if (grep(".tsv$|.txt$", ff)) {
          t0 <- read.delim(ff, stringsAsFactors = FALSE, header = TRUE)
        } else if (grep(".xlsx$", ff)) {
          t0 <- read.xlsx(ff, sheet = 1)
        } else {
          showModal(modalDialog(
            title = "Unknown file type",
            easyClose = TRUE
          ))
        }
        req(t0)
        pdr <- e2r(input$tab, conf$pdata)
        
        if (any(!t0$Label %in% pdr$Label) || any(!pdr$Label %in% t0$Label)) {
          showModal(modalDialog(
            title = "Phenotype data is not valid!",
            easyClose = TRUE
          ))
          req(NULL)
        }
        
        pdata( t0[match(pdr$Label, t0$Label), ] )
      })
      
      ## ================= protein info components ==================
      output$annot <- DT::renderDT({
        req( conf$fdata )
        formatDT( conf$fdata, pageLength = 20 )
      }, server = TRUE)
      
      # # =================== exclude info ========================
      observe(
        updateMultiInput(session, "exclude", choices = conf$label, selected = conf$sampleExclusion)
      )
      
      observeEvent(input$settingOrHelp, {
        updateTabsetPanel(session, "settingOrHelp_tabs", selected = paste0("tab_", input$settingOrHelp))
      })
      
      ## ================= return value ==================
      observeEvent(input$save, {
        req(dir())
        if ( file.exists( file.path( dirname(dir()), "ESVObj.RDS" )) ) {
          showModal(
            modalDialog(
              .msg$deleteConfirm[[lan]],
              footer = actionButton(ns("deleteOld"), label = .msg$delandgo[[lan]]), easyClose = TRUE
            )
          )
        }
      })
      
      observeEvent(input$deleteOld, {
        er <- file.path( dirname(dir()), "ESVObj.RDS" )
        unlink(er)
        shinyjs::click("save")
        removeModal()
      })
      
      observeEvent(input$save, {
        req(dir())
        req ( !file.exists(file.path(  dirname(dir()), "ESVObj.RDS" )) )
        
        trip <- list(
          exprs = conf$exprs,
          fdata = conf$fdata,
          pdata = e2r(input$tab, conf$pdata)
        )
        ff <- file.path(dirname(dir()), "obj.RDS")
        att <- list(
          filePath = ff,
          label = conf$label,
          sampleExclusion = input$exclude
        )
        attr(trip, "S1_dataLoading") <- att
        attr(trip, "label") <- NULL
        saveRDS(trip, file = ff)
        dat(trip)
      })
      
      dat
    }
  )
}

module_input_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
.modal-body .multi-wrapper {
        height: 500px;
}
.modal-body .non-selected-wrapper {
        height: 458px;
}
.modal-body .selected-wrapper {
        height: 458px;
}")),
    absolutePanel(top = -10, left = 275, tags$h2(.msg$loading[[lan]]), style = "z-index: 1111;"),
    absolutePanel(top = 5, right = 20, actionButton(inputId = ns("save"), .msg$saveset[[lan]]), style = "z-index: 1111;" ),
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
              tags$h3(.msg$expdesign[[lan]]),
              tabsetPanel(
                tabPanel(
                  .msg$editexpdesign[[lan]],
                  wellPanel(
                    style = "background: white; border-color: white",
                    fluidRow(
                      column(
                        6, align = "right",  style='padding-left:5px; padding-right:5px; padding-top:0px; padding-bottom:0px;',
                        downloadButton(outputId = ns("saveTemplate"), label = .msg$savetemplate[[lan]])
                      ),
                      column(
                        6, style='padding-left:5px; padding-right:5px; padding-top:0px; padding-bottom:0px;',
                        fileInput(inputId = ns("uploadTemplate"), label = NULL,
                                  placeholder = .msg$uploadtemplate[[lan]],
                                  multiple = FALSE, accept = c(".csv", ".tsv", "xlsx", "txt"))
                      ),
                      column(
                        12,
                        div(
                          tags$style("#l0-input-tab {border:none}"),
                          style = "overflow-x: scroll;", excelOutput(ns("tab"))
                        )
                      )
                    )
                  )),
                tabPanel(
                  .msg$delsample[[lan]],
                  wellPanel(
                    style = "background: white; border-color: white;",
                    multiInput(
                      inputId = ns("exclude"),
                      label = .msg$seldelsample[[lan]],
                      choices = "",
                      selected = NULL, width = "100%"
                    )
                  )
                )
              )
            )
          ),
          column(
            6,
            wellPanel(
              style = "background: white; height: 800px",
              tags$h3(.msg$featureinfo[[lan]]),
              DT::DTOutput(ns("annot"))
            )
          )
        )
      ),
      tabPanelBody(
        "tab_help",
        wellPanel(
          style = "background: white; height: 800px", 
          img(src='inst_01_setup.JPG', # height="750px", 
              style="display: block; margin-left: auto; margin-right: auto;")
        )
      )
    )
  )
}
