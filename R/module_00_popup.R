input_popup <- function(id, dir) {
  # ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      rt <- structure(normalizePath(unlist(dir)), names = names(dir))
      
      showModal(modalDialog(
        shinyFilesButton(
          id = ns('mqparOrYaml'), label = "Select input file", 
          title = "Acceptable file/format: .txt/.tsv", multiple = FALSE),
        title = "Loading project ...",
        footer = NULL, #modalButton("Open"),
        size = "m",
        easyClose = FALSE,
        fade = TRUE,
        style = "z-index: 9999"
      ))
      
      shinyFileChoose(
        input = input, 
        id = 'mqparOrYaml', 
        roots = rt,
        defaultRoot = names(rt)[1], 
        session = session, 
        filetypes=c('', "tsv", "txt"),
        restrictions = c("AnnotDB", "R-Portable-viewer")
      )
      
      observeEvent( input$mqparOrYaml, {
        req(!inherits(input$mqparOrYaml, "integer"))
        removeModal() 
      })
      
      reactive({
        req(input$mqparOrYaml)
        req(!inherits(input$mqparOrYaml, "integer"))
        v <- do.call(file.path, c(
          rt[[input$mqparOrYaml$root]], unlist(input$mqparOrYaml$files, recursive = FALSE)
        ))
        normalizePath(v)
      })
    }
  )}