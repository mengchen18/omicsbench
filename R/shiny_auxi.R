updateTabItemsBadge <- function(session, id, badgeLabel = "new", badgeColor = "green") {
  ns <- session$ns
  # nid <- ns(sprintf("#shiny-tab-%s", id))
  nid <- sprintf("#shiny-tab-%s", ns(id))
  jscode <- sprintf(
    'var els = document.querySelectorAll("a[href=\'%s\']")[0];
     var els = els.getElementsByClassName("badge")[0];
     els.className = "badge pull-right bg-%s"',
    nid,
    badgeColor
  )
  runjs(jscode)
  shinyjs::html(selector = sprintf('a[href=\'%s\'] > .badge', nid), add = FALSE, html = badgeLabel)
}


#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
readRDSWithSpinner <- function(...) {
  shinybusy::show_modal_spinner(text = .msg$loading2[[lan]])
  on.exit(
    shinybusy::remove_modal_spinner()
  )
  readRDS(...)
}


getQCStats <- function(x) {
  nval <- colSums(!is.na(x))
  nvalCum <- colSums(rowCumsums(apply(!is.na(x), 2, as.integer)) > 0)
  nvalInt <- colSums(!is.na(rowCumsums(x)))
  
  expr0 <- x
  expr0[is.na(expr0)] <- min(expr0, na.rm = TRUE) - log10(2)
  
  if (ncol(x) <= 2) {
    r1 <- r2 <- NULL
  } else {
    iir <- which(!is.na(rowSums(x)))
    if (length(iir) > 2) {
      expra <- x[iir, ]
      r1 <- omicsViewer::exprspca(expra, fillNA = FALSE, prefix = "") 
    } else
      r1 <- NULL
    r2 <- omicsViewer::exprspca(expr0, fillNA = FALSE, prefix = "")
  }
  
  list(
    nval = nval,
    nvalCum = nvalCum,
    nvalInt = nvalInt,
    pcNoImp = r1$samples,
    pcImp = r2$samples
  )
}

na2char <- function(x) {
  cc <- sapply(x, inherits, c("character", "factor"))
  for (i in which(cc)) {
    if (!is.character(x[, i]))
      x[, i] <- as.character(x[, i])
    ir <- is.na(x[, i])
    x[ir, i] <- ""
  }
  x
}

c2n <- function(x) {
  v <- as.numeric(as.character(x))
  if (is.na(v))
    return(NULL)
  v
}

formatDT <- function(tab, sel = c("single", "multiple")[1], pageLength = 15) {
  
  dt <- DT::datatable(
    na2char(tab),
    selection =  sel,
    rownames = FALSE,
    filter = "top",
    class="table-bordered compact nowrap",
    options = list(
      scrollX = TRUE, pageLength = pageLength, dom = 'tip', columnDefs = list(list(
        targets = unname(which(sapply(tab, inherits, c('factor', "character"))))-1,
        render = DT::JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 30 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
          "}")
      )))
  )
  DT::formatStyle(dt, columns = (1:ncol(tab))-1, fontSize = '90%')
}


# formatDTScrollY <- function(tab, sel = c("single", "multiple")[1], height = 500) {
  
#   dt <- DT::datatable( 
#     na2char(tab),
#     extensions = 'Scroller',
#     selection =  sel,
#     rownames = FALSE,
#     filter = "top",
#     class="table-bordered compact nowrap",
#     options = list(
#       scrollX = TRUE, scrollY = height, dom = 'ti', scroller = TRUE, 
#       pageLength = nrow(tab), columnDefs = list(list(
#         targets = unname(which(sapply(tab, inherits, c('factor', "character"))))-1,
#         render = DT::JS(
#           "function(data, type, row, meta) {",
#           "return type === 'display' && data.length > 30 ?",
#           "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
#           "}")
#       ))
#     )
#   )
#   DT::formatStyle(dt, columns = 1:ncol(tab), fontSize = '90%')
# }
formatDTScrollY <- function(tab, sel = c("single", "multiple")[1], height = 500) {
  
  dt <- DT::datatable( 
    na2char(tab),
    selection =  sel,
    rownames = FALSE,
    filter = "top",
    class="table-bordered compact nowrap",
    options = list(
      scrollX = TRUE, scrollY = height, dom = 'tip', scroller = FALSE, 
      pageLength = 20, columnDefs = list(list(
        targets = unname(which(sapply(tab, inherits, c('factor', "character"))))-1,
        render = DT::JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 30 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
          "}")
      ))
    )
  )
  DT::formatStyle(dt, columns = 1:ncol(tab), fontSize = '90%')
}


writeTriplet <- function(expr, pd, fd, file, creator) {
  
  td <- function(tab) {
    ic <- which(sapply(tab, is.list))
    if (length(ic) > 0) {
      for (ii in ic) {
        tab[, ii] <- sapply(tab[, ii], paste, collapse = ";")
      }
    }
    tab
  }
  
  wb <- createWorkbook(creator = creator)
  addWorksheet(wb, sheetName = "Phenotype info")
  addWorksheet(wb, sheetName = "Feature info")
  addWorksheet(wb, sheetName = "Expression")
  id <- paste0("ID", 1:nrow(expr))
  writeData(wb, sheet = "Expression", data.frame(ID = id, expr))
  writeData(wb, sheet = "Feature info", td(cbind(ID = id, fd)))
  writeData(wb, sheet = "Phenotype info", td(pd))
  saveWorkbook(wb, file = file, overwrite = TRUE)
}


getOriginDf <- function(x, rm.extra = "numberOfFeatures") {
  x <- x[, grep("^General", colnames(x)), drop = FALSE]
  colnames(x) <- str_split_fixed(colnames(x), "\\|", n = 3)[, 3]
  x <- x[, which(!colnames(x) %in% rm.extra), drop = FALSE]
  x
}


procConfigYaml <- function(x) {
  
  s1 <- attr(x, "S1_dataLoading")
  s2 <- attr(x, "S2_filterRow")
  s3 <- attr(x, "S3_normalization")
  s4 <- attr(x, "S4_annotation")
  s5 <- attr(x, "S5_misc")
  
  if (is.null(s5))
    return(NULL)
  
  n1 <- .msg$pending[[lan]]
  n2 <- .msg$pending[[lan]]
  if (!is.null(s2$index) && is.logical(s2$index)) {
    n1 <- sum(s2$index)
    n2 <- sum(!s2$index)
  }
  
  tagList(c(
    
    # 01 data loading
    list(
      tags$b(.msg$projectname[[lan]]),
      tags$p(basename(dirname(s1$filePath)))
    ),
    
    # 02 row filtering
    list(
      tags$b(.msg$filter[[lan]]),
      tags$p(sprintf(.msg$sum_quantile[[lan]], s2$rowMaxMinQuantile)),
      tags$p(sprintf(.msg$sum_groupvar[[lan]], s2$groupMinVar)),
      tags$p(sprintf(.msg$sum_groupmin[[lan]], s2$groupMinVarN)),
      tags$p(sprintf(.msg$sum_retainedfeature[[lan]], n1, n2))
    ),
    
    # 03 normalization
    list(
      tags$b(.msg$norm_col[[lan]]), #Column-wise normalization
      tags$p( s3$columnwise ),
      tags$b(.msg$norm_row[[lan]]),
      tags$p( s3$rowwise )
    ),
    
    # 04 annotation
    list(
      tags$b(.msg$annot_database[[lan]]),
      tags$p( basename(s4$annotationFile) ),
      tags$b(.msg$annot_database2[[lan]]), # Columns mapped to functional annotation file
      tags$p( paste(s4$annotationSource, collapse = "; " ) ),
      tags$b(.msg$sum_coldatabase[[lan]]), # Columns mapped to functional annotation file
      tags$p( s4$annotationColumn ),
      tags$b(.msg$sum_coldata[[lan]]), # Columns mapped to functional annotation file
      tags$p( s4$fdataColumn ),
      tags$b(.msg$annot_subsep[[lan]]), # Columns mapped to functional annotation file
      tags$p( s4$isoformSeparator )
    ),
    
    # 05 other 
    # string DB 
    list(
      tags$b(.msg$other_string[[lan]]), # Input column for STRING database query
      tags$p(s5$stringDbColumn)
    ),
    # Outlier
    list(
      tags$b(.msg$pcacheck[[lan]]),
      tags$p(s5$pca)
    ),
    # PTM
    list(
      tags$b(.msg$sum_motifcol[[lan]]),# PTM sequence window column
      lapply(s5$seqWindowColumn, tags$p)
    )
  ))
}

lfname <- function(x) {
  fs <- list.files(x, full.names = TRUE, pattern = ".annot$")
  names(fs) <- sub(".annot$", "", basename(fs))
  c("none" = "", fs)
}

methodText <- function(x) {
  s1 <- attr(x, "S1_dataLoading")
  s2 <- attr(x, "S2_filterRow")
  s3 <- attr(x, "S3_normalization")
  s4 <- attr(x, "S4_annotation")
  s5 <- attr(x, "S5_misc")
  
  sprintfnon <- function(x, ...) {
    s <- gsub("[\r\n]", "", sprintf(x, ...))
  }
  allValidValues <- function(...) {
    x <- list(...)
    j <- vapply(x, function(xx) {
      if (is.null(xx) || length(xx) == 0)
        return(FALSE)
      if (length(xx) > 1 || is.list(xx))
        return(TRUE)
      isTruthy(xx) & toupper(xx) != "NONE"
    }, FUN.VALUE = logical(1))
    if (length(j) > 1)
      j <- Reduce("&", j)
    j
  }
  
  tx <- c()
  if (allValidValues(s2)) {
    tx <- c(tx, "Data filtering")
    if ( any(!s2$index) ) {
      tx <- c(tx, sprintfnon(
        "In total, %s proteins were quantified. Data were filtered based on ...", 
        nrow(x$exprs)))
      
      if (allValidValues(s2$rowMaxMinQuantile))
        tx <- c(
          tx, 
          sprintfnon(
            "Data were filtered based on the maximum expression of proteins to remove 
low-intensity proteins, which usually have low signal-to-noise ratios. 
Expressly, if the maximum intensity of a protein (across samples) is lower 
than %s quantile of all protein intensities, then the protein is 
excluded from downstream analysis.", s2$rowMaxMinQuantile) )
      
      
      if ( allValidValues(s2$groupMinVar, s2$groupMinVarN) )
        tx <- c(
          tx, sprintfnon(
            "Data were filtered based on the missing values. The samples are divided into 
%s groups (%s), only proteins identified in more than %s samples in at least one of 
the groups are retained in the downstream analysis.",
            length(unique(x$pdata[, s2$groupMinVar])),
            paste(unique(x$pdata[, s2$groupMinVar]), collapse = ", "),
            s2$groupMinVarN
          )
        )
      
      cc <- table(s2$index)
      tx <- c(
        tx, 
        sprintfnon(
          "The filter removed %s proteins and retained %s proteins in the downstream analysis. 
The missing values were replaced by a constant two times lower than the lowest 
detected intensity. The rationale of this imputation is that the missing values 
are more likely to result from low abundant proteins.",
          cc["FALSE"], cc["TRUE"]
        ))
      
      tx <- c(
        tx, 
        "Then the protein intensities are logarithm transformed (base 10) in the next steps."
      )
    }
  }
  
  if (allValidValues(s3)) {
    tx <- c(tx, "Normalization")
    if ( allValidValues(s3$columnwise) )
      tx <- c(
        tx, 
        sprintfnon(
          "Considering the similarity between samples, the overall protein expression 
between samples should be comparable. Therefore, protein intensities across 
samples are %s to remove potential unwanted variances. ",
          switch(
            s3$columnwise,
            "Median centering" = "median centered",
            "Median centering (shared ID)" = "median centered based on the proteins identified in all sample",
            "Total sum" = "total sum normalized",
            "median centering + variance stablization" = "median centered and variance stabilized")
        ))
    
    if (allValidValues(s3$rowwise) )
      tx <- c(
        tx, 
        sprintfnon(
          "To remove the potential batch effects, %s",
          switch (
            s3$rowwise,
            "Reference" = "(briefly describe how the reference samples are measured, how many samples ...). 
    The variance in the reference samples are removed to correct the potential batch effect in the data.",
            "Batch mean" = "the mean intensities of each protein in the different batches are aligned.",
            "Batch reference" = "the protein intensities of reference samples from the different batches are aligned. "
          )
        ))
  }
  
  if (allValidValues(s4)) {
    tx <- c(tx, "Annotation")
    
    upid <- sapply(strsplit(basename(s4$annotationFile), "_"), "[", 1)
    
    if (allValidValues(s4$annotationSource, grep("^UP0", upid)))
      tx <- c(
        tx, 
        sprintfnon(
          "The ene set annotation were compiled from UniProt database (%s), which 
collects data originally from %s [please cite the relevent publications]",
          upid,
          paste(s4$annotationSource, collapse = ", ")
        )
      )
  }
  tx <- c(
    tx, 
    sprintf("All analyses were performed using R (%s)", R.version.string) 
  )
  
  
  "The Gene ontology terms and other gene set annotation information were predicted using interproscan [CITE: PMID: 24451626] and PANZZER2 [CITE: PMID: 34562305]. "
  
  lapply(tx, tags$p)
}

