#' UI
#' @export
#' @import stringr
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable, runExample))
#' @import shinydashboard
#' @importFrom stats lm median na.omit quantile
#' @importFrom utils combn download.file head read.csv read.delim tail write.table
#' @importFrom password password
#' @rawNamespace import(shinyWidgets, except = alert)
#' @import shinybusy
#' @import matrixStats
#' @import flatxml
#' @import excelR
#' @import shinyjs
#' @import shinyFiles
#' @import DT
#' @import plotly
#' @import openxlsx
#' @import yaml
#' @import curl
#' @importFrom omicsViewer nColors rowshift normalize.nQuantiles normalize.totsum removeVarQC normalizeData normalizeColWise filterRow 
#' @rawNamespace import(Biobase, except = c(show, anyMissing, rowMedians))
#' @import sortable
#' @import omicsViewer
#' @param id id

cruncher_ui <- function(id) {
  ns <- NS(id)
  
  dashboardPage(
    dashboardHeader(title = "OmicsBench"),
    dashboardSidebar(
      useShinyjs(),
      sidebarMenu(
        id = ns("tabs"),
        menuItem(.msg$loading[[lan]], tabName = ns("loading"), icon = icon("file-import"), badgeLabel = .msg$waitset[[lan]], badgeColor = "purple"),
        menuItem(.msg$filter[[lan]], tabName = ns("filterprotein"), icon = icon("filter"), badgeLabel = .msg$waitset[[lan]], badgeColor = "purple"),
        menuItem(.msg$datanorm[[lan]], tabName = ns("normalization"), icon = icon("align-center"), badgeLabel = .msg$waitset[[lan]], badgeColor = "purple"),
        menuItem(.msg$funannot[[lan]], tabName = ns("annot"), icon = icon("info-circle"), badgeLabel = .msg$waitset[[lan]], badgeColor = "purple"),
        menuItem(.msg$otherset[[lan]], tabName = ns("misc"), icon = icon("list-alt"), badgeLabel = .msg$waitset[[lan]], badgeColor = "purple"),
        menuItem(.msg$reviewset[[lan]], tabName = ns("review"), icon = icon("calendar-check")),
        menuItem(
          .msg$stats[[lan]], tabName = ns("stats"), icon = icon("calculator"), startExpanded = TRUE,
          menuItem(
            .msg$ttest[[lan]], tabName = ns("ttest"),  icon = icon("not-equal"), badgeLabel = .msg$notanalysed[[lan]], badgeColor = "purple"
          ),
          menuItem(
            .msg$cortest[[lan]], tabName = ns("cortest"),  icon = icon("chart-line"), badgeLabel = .msg$notanalysed[[lan]], badgeColor = "purple"
          ),
          menuItem(
            .msg$outliertest[[lan]], tabName = ns("outlier"),  icon = icon("star-half-alt"), badgeLabel = .msg$notanalysed[[lan]], badgeColor = "purple"
          ),
          menuItem(
            .msg$stepupdown[[lan]], tabName = ns("steps"),  icon = icon("times"), badgeLabel = .msg$notanalysed[[lan]], badgeColor = "purple"
          )
        ),
        menuItem(.msg$checkres[[lan]], tabName = ns("viewer"), icon = icon("check-square"), badgeLabel = .msg$nores[[lan]], badgeColor = "purple")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = ns("loading"),
          module_input_ui(id = ns("input"))
        ),
        tabItem(
          tabName = ns("filterprotein"),
          module_filterRow_ui(ns("body_filterprotein"))
        ),
        tabItem(
          tabName = ns("normalization"),
          module_normalization_ui(ns("body_normalization"))
        ),
        tabItem(
          tabName = ns("review"),
          module_paramReview_ui(ns("settingReview"))
        ),
        tabItem(
          tabName = ns("ttest"),
          module_ttest_ui(ns("stats_ttest"))
        ),
        tabItem(
          tabName = ns("outlier"),
          module_outlier_ui(ns("stats_outlier"))
        ),
        tabItem(
          tabName = ns("cortest"),
          module_cortest_ui(ns("stats_cortest"), tabName = ns("cortest")) ## careful
        ),
        tabItem(
          tabName = ns("steps"),
          module_steps_ui(ns("stats_steps"))
        ),
        tabItem(
          tabName = ns("annot"),
          module_annot_ui(ns("prot_annot"))
        ),
        tabItem(
          tabName = ns("misc"),
          module_submit_ui(ns("misc"))
        ),
        tabItem(
          tabName = ns("viewer"),
          tags$style('.container-fluid { margin: 0px; padding: 0px; } #shiny-tab-l0-viewer {margin-top:-10px} .navbar {margin-right:-15px; margin-left:-5px}'),
          absolutePanel(top = -10, left = 275, tags$h2(.msg$checkres[[lan]]), style = "z-index: 1111;"),
          omicsViewer:::app_ui(ns("app"), showDropList = TRUE)
        )
      )
    )
  )
}

#' Server
#' @param id id
#' @param userinfo the user information, including - dir, type, typeEnd, addno
#' @param annot_dir the directory for annotation databases (global), the project specific annotation is in the .gsannot folder
#' @param demoData path to the demo data file
#' @param project_dir project dir
#' @param ... not used currently
#' @export
#' 
cruncher_server <- function(id, project_dir, annot_dir, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # ======================= internal utilities ========================
      updateBadge <- function(id, v = TRUE) {
        if (v)
          updateTabItemsBadge(session, id, badgeLabel = .msg$saved[[lan]], badgeColor = "green") else
            updateTabItemsBadge(session, id, badgeLabel = .msg$skip[[lan]], badgeColor = "maroon")
      }
      validValue <- function(x) {
        if (inherits(x, "logical"))
          return(x)
        !is.null(x) && length(x) > 0 && !x %in% c("", "none")
      }
      validValues <- function(...) {
        al <- list(...)
        all(sapply(al, validValue))
      }
      output$style_backgroundColor <- renderUI({
        if(input$tabs != 'viewer')
          return(tags$head(tags$style(HTML('.content-wrapper {background-color:#F5F5F5;}'))))
        tags$head(tags$style(HTML('.content-wrapper {background-color:white;}')))
      })
      session$onSessionEnded(function() {
        if (tolower(Sys.info()['sysname']) != "windows")
          return(NULL)
        stopApp()
      })
      
      # ======================= update badge depends on obj ========================
      obj <- reactiveVal()
      esv <- reactiveVal()
      
      observeEvent(list(obj(), esv()), {
        
        req( obj() )
        show_modal_spinner( text= .msg$loading2[[lan]])
        # Sys.sleep(2)
        if (is.null(j <- attr(obj(), "S1_dataLoading")))
          updateTabItemsBadge(session, "loading", badgeLabel = .msg$waitset[[lan]], badgeColor = "purple") else
            updateBadge("loading", length(j) >= 2 )
        if (is.null(j <- attr(obj(), "S2_filterRow")))
          updateTabItemsBadge(session, "filterprotein", badgeLabel = .msg$waitset[[lan]], badgeColor = "purple") else
            updateBadge("filterprotein", !is.null(j$index) )
        if (is.null(j <- attr(obj(), "S3_normalization")))
          updateTabItemsBadge(session, "normalization", badgeLabel = .msg$waitset[[lan]], badgeColor = "purple") else
            updateBadge("normalization", length(j) == 2)
        if (is.null(j <- attr(obj(), "S4_annotation")))
          updateTabItemsBadge(session, "annot", badgeLabel = .msg$waitset[[lan]], badgeColor = "purple") else
            updateBadge("annot", validValues(j$annotationFile, j$annotationColumn, j$fdataColumn, j$annotationSource))
        if (is.null( j <- attr(obj(), "S5_misc")) )
          updateTabItemsBadge(session, "misc", badgeLabel = .msg$waitset[[lan]], badgeColor = "purple") else
            updateBadge("misc", length(j) == 3)
        
        if ( is.null( j <- attr(esv(), "S6.1_ttest")) ) 
          updateTabItemsBadge(session, "ttest", badgeLabel = .msg$notanalysed[[lan]], badgeColor = "purple") else
            updateBadge("ttest", nrow(j) > 0)
        
        if ( is.null( j <- attr(esv(), "S6.2_outlier")) ) 
          updateTabItemsBadge(session, "outlier", badgeLabel = .msg$notanalysed[[lan]], badgeColor = "purple") else
            updateBadge("outlier", j)
        
        if (is.null( j <- attr(esv(), "S6.3_cortest") ))
          updateTabItemsBadge(session, "cortest", badgeLabel = .msg$notanalysed[[lan]], badgeColor = "purple") else
            updateBadge("cortest", length(j) > 0 )
        
        if (is.null(j <- attr(esv(), "S6.4_step")))
          updateTabItemsBadge(session, "steps", badgeLabel = .msg$notanalysed[[lan]], badgeColor = "purple") else
            updateBadge("steps", length(j) > 0 )

        ff <- file.exists(file.path( dirname(attr(obj(), "S1_dataLoading")$filePath), "ESVObj.RDS" ))
        if (ff)  updateTabItemsBadge(session, "viewer", badgeLabel = .msg$ready[[lan]], badgeColor = "green") else
          updateTabItemsBadge(session, "viewer", badgeLabel = .msg$nores[[lan]], badgeColor = "purple")
        
        remove_modal_spinner()
      })
      # 
      
      
      ###################### data loading #########################
      path <- list(...)
      if (length(path) == 0) {
        mqparPath <- input_popup("id", dir = project_dir)
      } else {
        mqparPath <- path$path
        updateTabItems(session, inputId = "tabs", selected = ns("viewer"))
      }
      #     
      # ###################### modules start #########################
      #     
      d01 <- module_input( id = "input", dir = mqparPath)
      observeEvent(d01(), {
        req(d01())
        obj( d01() )
        req(v <- attr(d01(), "S1_dataLoading"))
        ff <- file.path( dirname(v$filePath), "ESVObj.RDS" )
        atab <- ifelse ( file.exists(ff), "viewer", "filterprotein" )
        updateTabItems(session, inputId = "tabs", selected = ns(atab))
      })
      #     
      d02.1 <- module_filterRow(id = "body_filterprotein", obj = obj)
      observeEvent(d02.1(), {
        req(d02.1())
        obj( d02.1() )
        updateTabItems(session, inputId = "tabs", selected = ns("normalization"))
      })
      #     
      d02.2 <- module_normalization(id = "body_normalization", obj = obj)
      observeEvent(d02.2(), {
        req( d02.2() )
        obj( d02.2() )
        updateTabItems(session, inputId = "tabs", selected = ns("annot"))
      })
      #     
      d03 <- module_annot(id = "prot_annot", obj = obj, annot_dir_global = normalizePath(annot_dir))
      observeEvent(d03(), {
        obj( d03() )
        updateTabItems(session, inputId = "tabs", selected = ns("misc"))
      })
      #     
      d05 <- module_submit(id = "misc", obj = obj)
      observeEvent(d05(), {
        obj(d05())
        updateTabItems(session, inputId = "tabs", selected = ns("review"))
      })
      #     
      observe({
        req(v <- attr(obj(), "S1_dataLoading"))
        ff <- file.path( dirname(v$filePath), "ESVObj.RDS" )
        if ( file.exists(ff) ) {
          esv( readRDSWithSpinner(ff) ) 
        } else 
          esv ( NULL )
      })
      #     
      res0 <- module_paramReview(id = "settingReview", obj = obj)
      observeEvent(res0(), {
        if (!is.null(m <- attr(res0(), "message"))) {
          showModal(modalDialog(
            lapply(paste0(1:length(m), ". ", m), tags$p),
            title = .msg$problem[[lan]],
            footer = modalButton(.msg$ok[[lan]])
          ))
        }
        esv(res0())
      })
      #     
      d03.1 <- module_ttest(id = "stats_ttest", esv = esv)
      observeEvent(d03.1(), {
        esv( d03.1() )
        updateTabItems(session, inputId = "tabs", selected = ns("viewer"))
      })
      
      d03.4 <- module_outlier(id = "stats_outlier", esv = esv)
      observeEvent(d03.4(), {
        esv( d03.4() )
        updateTabItems(session, inputId = "tabs", selected = ns("viewer"))
      })
      #     
      d03.2 <- module_cortest(id = "stats_cortest", esv = esv)
      observeEvent(d03.2(), {
        esv( d03.2() )
        updateTabItems(session, inputId = "tabs", selected = ns("viewer"))
      })
            
      d03.3 <- module_steps(id = "stats_steps", esv = esv)
      observeEvent(d03.3(), {
        req(d03.3())
        esv( d03.3() )
        updateTabItems(session, inputId = "tabs", selected = ns("viewer"))
      })
      
      callModule(
        omicsViewer:::app_module, id = "app",
        .dir = reactive({
          req( mqparPath() )
          dirname(mqparPath())
          }),
        additionalTabs = NULL,  filePattern = ".RDS", # don't need to change
        ESVObj = esv, 
        exprsGetter = exprs, pDataGetter = pData, fDataGetter = fData,
        defaultAxisGetter = function(x, what = c("sx", "sy", "fx", "fy")[1]) attr(x, what),
        appName = NULL, appVersion = NULL)
    }
  )
}

