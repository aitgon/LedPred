#source("Model.R")

ModelPerformance <- R6::R6Class(
  "ModelPerformance",
  inherit = Model,
  public = list(
    x = NA,
    y = NULL,
    valid.times = 1,
    test.folds = NA,
    model = NA,
    model.obj = NA,
    weights = NA,
    numcores = parallel::detectCores() - 1,
	file.prefix = "",
    cv.probs.labels = NA,
    feature.ranking = NULL,
    feature.nb = NULL,
    initialize = function(x, y, valid.times, numcores, file.prefix=self$file.prefix, feature.ranking=self$feature.ranking, feature.nb=self$feature.nb) {
      self$model.obj = Model$new(x = x, y = y, valid.times = valid.times, feature.ranking=feature.ranking, feature.nb=feature.nb)
      self$x = self$model.obj$x
      self$y = self$model.obj$y
      self$valid.times = self$model.obj$valid.times
      self$test.folds = self$model.obj$test.folds
      self$model = self$model.obj$model
      self$weights = self$model.obj$weights
      self$feature.ranking = self$model.obj$feature.ranking
      self$feature.nb = self$model.obj$feature.nb
      if (!missing(numcores)) self$numcores = numcores
      if (!missing(file.prefix)) self$file.prefix = file.prefix
      self$cv.probs.labels = private$CVModelPeformanceAllFolds(test.folds = self$test.folds)
      png(paste(self$file.prefix,"_ROC_perf.png", sep=""))
      private$PlotROCR(
        labels = self$cv.probs.labels$labels, probs = self$cv.probs.labels$probs
      )
      garb = dev.off()
    },
    Tests = function() {
	probs=private$CVModelPeformanceOneFold(test.fold.i=self$test.folds[[1]])$probs
	testthat::expect_equal(probs[[1]], 0.9748439, tolerance=10-9)
    }
  ),
  private = list(
    CVModelPeformanceOneFold = function(test.fold.i = test.fold.i) {
      test.set.x = self$x[test.fold.i,]
      train.set.x = self$x[-test.fold.i,]
      test.set.y = self$y[test.fold.i]
      train.set.y = self$y[-test.fold.i]
      obj <- Model$new(x = train.set.x, y = train.set.y, valid.times = 5)
      probs = obj$ScoreData(x = test.set.x)$probs
      return(list(probs = probs, labels = test.set.y))
    },
    CVModelPeformanceAllFolds = function(test.folds = test.folds) {
      cv.probs.labels = parallel::mclapply(test.folds, private$CVModelPeformanceOneFold, mc.cores=self$numcores)
      labels = sapply(cv.probs.labels, function(x)
        x$labels)
      probs = sapply(cv.probs.labels, function(x)
        x$probs)
      return(list(labels = labels, probs = probs))
    },
    PlotROCR = function (labels = labels, probs = probs) {
      rocr.pred = ROCR::prediction(probs,labels,label.ordering = c(-1, 1))
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
  )
)
