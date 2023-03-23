stepAnalysis <- function(expr, pdata, steps) {
  
  tl <- lapply(steps, function(cc) {
    pname <- cc[[1]]
    gname <- unlist(cc[-1])
    n <- length(gname)
    t(sapply(1:(n-1), function(i) {
      c(pname, gname[i:(i+1)])
    }))
  })
  tl <- do.call(rbind, tl)
  tl <- unique(tl)
  colnames(tl) <- NULL
  
  dd <- prepOmicsViewer(
    expr = expr, pData = pdata, fData = data.frame(placeholder = 1:nrow(expr)), 
    PCA = FALSE, pca.fillNA = TRUE, t.test = tl, ttest.fillNA = TRUE,
    SummarizedExperiment = FALSE)
  
  # steps up/down
  fd <- fData(dd)
  sts <- getSteps(fdata = fd, stepList = steps)
  cbind(fd[, -1], sts)
}