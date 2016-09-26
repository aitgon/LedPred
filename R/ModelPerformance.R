#source("Model.R")

ModelPerformance <- R6::R6Class(
  "ModelPerformance",
  inherit = Model,
  public = list(
    model.obj = NULL,
    cv.probs.labels = NULL,
    auc = NULL,
    initialize = function(x, y, kernel = self$kernel, cost = self$cost, gamma =
                            self$gamma, valid.times = self$valid.times, numcores = self$numcores, file.prefix =
                            self$file.prefix, feature.ranking = self$feature.ranking, feature.nb = self$feature.nb) {
      if (!missing(kernel))
        self$kernel = kernel
      if (!missing(cost))
        self$cost = cost
      if (!missing(gamma))
        self$gamma = gamma
      if (!missing(valid.times))
        self$valid.times = valid.times
      if (!missing(numcores))
        self$numcores = numcores
      if (!missing(feature.ranking))
        self$feature.ranking = feature.ranking
      if (!missing(feature.nb))
        self$feature.nb = feature.nb
      if (!missing(file.prefix))
        self$file.prefix = file.prefix
      self$model.obj = Model$new(
        x = x, y = y, kernel = self$kernel, cost = self$cost, gamma = self$gamma, valid.times = self$valid.times, feature.ranking =
          self$feature.ranking, feature.nb = self$feature.nb, file.prefix = self$file.prefix
      )
      self$x = self$model.obj$x
      self$y = self$model.obj$y
      self$test.folds = self$model.obj$test.folds
      self$model = self$model.obj$model
      self$weights = self$model.obj$weights
      self$cv.probs.labels = private$CVModelPeformanceAllFolds(test.folds = self$test.folds)
      try(png(paste(self$file.prefix,"_ROC_perf.png", sep = "")))
      private$PlotROCR(
        labels = self$cv.probs.labels$labels, probs = self$cv.probs.labels$probs, rocr.pred = self$cv.probs.labels$rocr.pred
      )
      garb = dev.off()
    },
    Tests = function() {
      probs = private$CVModelPeformanceOneFold(test.fold.i = self$test.folds[[1]])$probs
      testthat::expect_equal(probs[[1]], 0.9748439, tolerance = 10 - 9)
    }
  ),
  private = list(
    CVModelPeformanceOneFold = function(test.fold.i = test.fold.i) {
      test.set.x = self$x[test.fold.i,]
      train.set.x = self$x[-test.fold.i,]
      test.set.y = self$y[test.fold.i]
      train.set.y = self$y[-test.fold.i]
      
      # Remove empty features because it triggers model errror
      if (any(apply(train.set.x==0,2,all))) {
        warning("Some features in this fold are zero everywhere and must be removed")
        train.set.x = train.set.x[, !apply(train.set.x==0,2,all)]
        test.set.x = test.set.x[, colnames(train.set.x)]
      }
      # End of - Remove empty features because it triggers model errror
      
      obj <-
        Model$new(
          x = train.set.x, y = train.set.y, kernel = self$kernel, cost = self$cost, gamma =
            self$gamma, valid.times = 5
        )
      probs = obj$ScoreData(x = test.set.x, scale=FALSE)$probs
      return(list(probs = probs, labels = test.set.y))
    },
    CVModelPeformanceAllFolds = function(test.folds = test.folds) {
    message("evaluateModelPerformance is running ...")
      cv.probs.labels = parallel::mclapply(test.folds, private$CVModelPeformanceOneFold, mc.cores =
                                             self$numcores)
      labels = sapply(cv.probs.labels, function(x)
        x$labels)
      probs = sapply(cv.probs.labels, function(x)
        x$probs)
        #Calculate AUC using ROCR package ----------------
 rocr.pred = ROCR::prediction(probs,labels,label.ordering = c(-1, 1))
      auc.tmp = ROCR::performance(rocr.pred, "auc");
      self$auc = round(mean(as.numeric(auc.tmp@y.values)), digits = 2)
        #End of Calculate AUC using ROCR package ----------------
      return(list(labels = labels, probs = probs, rocr.pred= rocr.pred))
    },
    PlotROCR = function (labels = labels, probs = probs, rocr.pred= rocr.pred) {
#      rocr.pred = ROCR::prediction(probs,labels,label.ordering = c(-1, 1))
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
