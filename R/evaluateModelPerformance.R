evaluateModelPerformance <-
  function(data = data, valid.times = 5, numcores = parallel::detectCores() - 1, feature.ranking = NULL, feature.nb =NULL, file.prefix = NULL) {

    if (!is.null(feature.ranking) && !is.null(feature.nb)) {
      cl <- data$cl
      selected.features <- feature.ranking[1:feature.nb,1]
      data <- data[,names(data) %in% selected.features]
      data <- data.frame(cl = cl, data)
    }

    testi.vector <- .makeCrossValidSets(data, valid.times)
    cv.probs.labels = parallel::mclapply(testi.vector, .calculatePredictionProbability, data)
    if (!is.null(file.prefix)) {
      labels = sapply(cv.probs.labels, function(x)
        x$labels)
      probs = sapply(cv.probs.labels, function(x)
        x$probs)
      png(paste(file.prefix,"_ROC_perf.png",sep = ""))
      .plotRocr(labels, probs)
      garb = dev.off()
    }
    return(cv.probs.labels)
  }

.calculatePredictionProbability = function(testi, data = data, cl = 1) {
  trainset = data[-testi,]
  testset = data[testi,]

  model <- createModel(trainset, cl = 1)

  library(e1071)
  classpred = predict(model, testset[,-1], decision.values = T, probability = TRUE)
  probs = attr(classpred,"decision.values")[,1]
  labels = testset$cl
  return (list(probs = probs, labels = labels))
}

.plotRocr = function(labels, probs) {
  rocr.pred = ROCR::prediction(probs,labels,label.ordering = c(-1, 1))
  # These are the 4 performance measures in the plot
  prec_rec = ROCR::performance(rocr.pred, "prec", 'rec')
  prec = ROCR::performance(rocr.pred, measure = "prec")
  rec = ROCR::performance(rocr.pred, measure = "rec")
  tpr_fpr = ROCR::performance(rocr.pred, measure = "tpr", x.measure = "fpr")
  auc.tmp = ROCR::performance(rocr.pred, "auc");
  auc = round(mean(as.numeric(auc.tmp@y.values)), digits = 2)
  par(
    mfrow = c(2,2), oma = c(1,1,1,1), mar = c(5, 4.5, 0, 2) + 0.1
  )
  par(cex.lab = 1.5)
  ROCR::plot(
    prec, avg = "vertical", spread.estimate = "stderror", col = c("#FF0000FF"), ylim =
      c(0,1),xlim = c(0,1)
  )
  ROCR::plot(
    rec, avg = "vertical",spread.estimate = "stderror",col = c("#FF0000FF"),ylim =
      c(0,1),xlim = c(0,1)
  )
  ROCR::plot(
    prec_rec, avg = "vertical", spread.estimate = "stderror", col = c("#FF0000FF"), ylim =
      c(0,1), xlim = c(0,1)
  )
  ROCR::plot(
    tpr_fpr, avg = "vertical",spread.estimate = "stderror",col = c("#FF0000FF"),ylim =
      c(0,1),xlim = c(0,1)
  )
  text(0.7, 0.2, paste("AUC= ", auc, sep = ""),cex = 1.2)
}
