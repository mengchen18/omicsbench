#' Internal function to create object for ESV
#' @param x configured object
# v <- prepESVObj(a)
# omicsViewer("~/projects/SXLj_GtNH9g0yKFCq5f2wY3NqCrBVhpR5uQB/P03/")

prepESVObj <- function(x) {
  
  message <- c()
  
  s1 <- attr(x, "S1_dataLoading")
  s2 <- attr(x, "S2_filterRow")
  s3 <- attr(x, "S3_normalization")
  s4 <- attr(x, "S4_annotation")
  s5 <- attr(x, "S5_misc")
  
  # filtering rows and columns
  ic <- which(!s1$label %in% s1$sampleExclusion)
  ir <- which(s2$index)
  exprs <- x$exprs[ir, ic, drop = FALSE]
  pdata <- x$pdata[ic, , drop = FALSE]
  fdata <- x$fdata[ir, , drop = FALSE]
  
  # normalization
  d0 <- normalizeData(
    exprs, colWise = s3$columnwise, rowWise = s3$rowwise, ref = pdata$Reference, batch = pdata$Batch
  )
  i <- which(rowSums(!is.na(d0)) > 0)
  exprs <- d0[i, , drop = FALSE]
  fdata <- fdata[i, , drop = FALSE]
  
  # gene set annotation
  gs <- NULL
  if (!is.null(s4$annotationFile) & length(s4$annotationSource) > 0) {
    # if annot file is given then annotate
    gsannot <- read.delim(s4$annotationFile, stringsAsFactors = FALSE, sep = "\t")
    gsannot <- gsannot[gsannot$source %in% s4$annotationSource, ]
    
    if (s4$annotationColumn %in% colnames(gsannot) && s4$fdataColumn %in% colnames(fdata)) {
      terms <- data.frame(
        id = gsannot[[s4$annotationColumn]],
        term = paste(gsannot$source, trimws(gsannot$term), trimws(substr(gsannot$desc, 1, 50)), sep = "_"),
        stringsAsFactors = FALSE
      )
      id <- strsplit(fdata[[s4$fdataColumn]], split = ";")
      if ( !is.null(s4$isoformSeparator) && s4$isoformSeparator %in% c("dot (.)", "dash (-)")) {
        if ( s4$isoformSeparator == "dot (.)" )
          separator <- "\\." else
            separator <- "-"
          id <- lapply(id, function(x) sapply(strsplit(x, separator), "[", 1))
      }
      gs <- try(gsAnnotIdList(idList = id, gsIdMap = terms), silent = TRUE)      
      if (inherits(gs, 'try-error') || ncol(gs) == 0) {
        message <- c(
          message, 
          .msg$annot_nomatch[[lan]]
          )
        gs <- NULL
      }
    }
  }
  
  sdbid <- NULL
  if (s5$stringDbColumn %in% colnames(fdata))
    sdbid <- sapply(strsplit(fdata[[s5$stringDbColumn]], ";"), "[", 1)
  
  dd <- prepOmicsViewer(
    expr = exprs, pData = pdata, fData = fdata, PCA = TRUE, pca.fillNA = TRUE, stringDB = sdbid, gs = gs,
    SummarizedExperiment = FALSE
    )
  gs <- attr(fData(dd), "GS")
  
  ## PTM or seq logo columns
  if (!is.null(s5$seqWindowColumn) && s5$seqWindowColumn != "none" && s5$seqWindowColumn %in% colnames(fdata)) {
    
    fd <- fData(dd)
    adf <- fdata[, s5$seqWindowColumn, drop = FALSE]
    colnames(adf) <- paste("SeqLogo", "All", colnames(adf), sep = "|")
    fd <- cbind(fd, adf)
    fData(dd) <- fd
    
    # fd <- fData(dd)
    # ptmVar <- fdata[, s5$seqWindowColumn, drop = FALSE]
    # colnames(ptmVar) <- paste("SeqLogo", "All", colnames(ptmVar), sep = "|")
    # fd <- cbind(fd, ptmVar)
    # 
    # sameLen <- vapply(ptmVar, function(x) {
    #   ss <- unlist(strsplit(x, ";"))
    #   ss <- na.omit(ss)
    #   ss <- ss[which(nchar(ss) > 0)]
    #   length(unique(nchar(ss))) == 1
    #   }, logical(1))
    # 
    # if (!all(sameLen)) {
    #   message <- c(
    #     message, 
    #     "SeqLogo 分析需要长度相同的肽段序列，再序列的中间是被修饰的氨基酸。而您所选择的列包含不同长度的肽段序列！"
    #   )
    # } else {
    #   attr(dd, "ptm.seq.window") <- omicsViewer:::aaFreq( l ) ##
    # }
    # fData(dd) <- fd
  }
  
  # set default axis
  vv <- Biobase::fData(dd)
  attr(vv, "GS") <- gs
  fData(dd) <- vv
  tsr <- grep("ttest" , colnames(vv), value = TRUE)
  if (length(tsr) >= 5) {
    fx <- tsr[5]
    fy <- tsr[4]
  } else if (any(grepl("PCA", colnames(vv)))) {
    fx <- grep("PCA", colnames(vv), value = TRUE)[1] 
    fy <- grep("PCA", colnames(vv), value = TRUE)[2] 
  } else
    fx <- fy <- NULL
  # feature space - default x axis
  attr(dd, "fx") <- fx
  # feature space - default y axis
  attr(dd, "fy") <- fy
  
  # sample space - default x axis
  attr(dd, "sx") <- "PCA|All|PC1("
  # sample space - default y axis
  attr(dd, "sy") <- "PCA|All|PC2("
  attr(dd, "message") <- message
  
  outputFile <- file.path(dirname(s1$filePath), "ESVObj.RDS")
  attr(dd, "filePath") <- outputFile
  saveRDS(dd, file = outputFile)
  
  invisible(dd)
}



