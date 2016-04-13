LedPred = function(x, y, ...) {
  obj <- LedPredClass$new(x = x, y = y, ...)
  ledpred.summary <-
    list(
      feature.ranking = obj$feature.ranking, feature.nb =
        obj$best.feature.nb, model.obj = obj$model.obj, test.folds = obj$test.folds, probs.label.list = obj$probs.label.list
    )
  return(ledpred.summary)
}

LedPredClass <- R6::R6Class(
  "LedPredClass",
  inherit = Data,
  public = list(
    kfold.nb = 1,
    halve.above = 100,
    feature.ranking = NULL,
    feature.nb.vector = NULL,
    feature.performances = NULL,
    best.feature.nb = NULL,
    model = NULL,
    model.obj = NULL,
    weights = NULL,
    cv.probs.labels = NULL,
    initialize = function(x, y, valid.times = self$valid.times, kfold.nb =
                            self$kfold.nb, halve.above = self$halve.above, numcores = self$numcores, file.prefix =
                            self$file.prefix, feature.nb.vector, cost=self$cost, gamma=self$gamma, kernel=self$kernel) {
      #
      if (!missing(valid.times))
        self$valid.times <- valid.times
      if (!missing(kfold.nb))
        self$kfold.nb <- kfold.nb
      if (!missing(numcores))
        self$numcores <- numcores
      if (!missing(file.prefix))
        self$file.prefix <- file.prefix
      if (!missing(cost))
        self$cost <- cost
      if (!missing(gamma))
        self$gamma <- gamma
      if (!missing(kernel))
        self$kernel <- kernel
      #
      feature.ranking.obj <-
        FeatureRanking$new(
          x, y, valid.times = self$valid.times, kfold.nb = self$kfold.nb, halve.above =
            self$halve.above, numcores = self$numcores, file.prefix = file.prefix, cost=self$cost, gamma=self$gamma, kernel=self$kernel
        )
      #
      self$x = feature.ranking.obj$x
      self$y = feature.ranking.obj$y
      self$test.folds = feature.ranking.obj$test.folds
      # self$scale.factors = feature.ranking.obj$scale.factors
#      self$scale.center = feature.ranking.obj$scale.center
#      self$scale.scale = feature.ranking.obj$scale.scale
      self$feature.ranking = feature.ranking.obj$feature.ranking
      #
      self$feature.nb.vector = feature.nb.vector
      #
      feature.nb.tuner.obj <-
        FeatureNbTuner$new(
          x = self$x, y = self$y, valid.times = self$valid.times, numcores = self$numcores, feature.ranking =
            self$feature.ranking, feature.nb.vector = self$feature.nb.vector, file.prefix =
            self$file.prefix, cost=self$cost, gamma=self$gamma, kernel=self$kernel
        )
      #
      self$feature.performances = feature.nb.tuner.obj$feature.performances
      self$best.feature.nb = feature.nb.tuner.obj$best.feature.nb
      #
      model.perf.obj = ModelPerformance$new(
        x = x, y = y, feature.ranking = self$feature.ranking, feature.nb = self$best.feature.nb, file.prefix =
          self$file.prefix, cost=self$cost, gamma=self$gamma, kernel=self$kernel
      )
      ##    #
      self$model = model.perf.obj$model
      self$model.obj = model.perf.obj$model.obj
      self$weights = model.perf.obj$weights
      self$cv.probs.labels = model.perf.obj$cv.probs.labels
      if (!is.null(self$file.prefix))
        save(self, file = paste(file.prefix, '_ledpred.rda', sep = ""))
      #
      #    print(self$feature.ranking)
      #    print(self$best.feature.nb)
      #    print(rownames(self$model$SV)[1:3])
      #    print(self$cv.probs.labels$probs[1,1])
    }
  )
)
