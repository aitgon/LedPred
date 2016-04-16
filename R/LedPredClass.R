LedPredClass <- R6::R6Class(
  "LedPredClass",
  inherit = ParameterTuner,
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
    ranges = list(gamma=c(1,10), cost=c(1,10)),
    initialize = function(x, y, valid.times = self$valid.times, kfold.nb =
                            self$kfold.nb, halve.above = self$halve.above, numcores = self$numcores, file.prefix =
                            self$file.prefix, feature.nb.vector, cost=self$cost, gamma=self$gamma, ranges = self$ranges, kernel=self$kernel) {
      #
      if (!missing(kernel))
        self$kernel = kernel
      if (!missing(cost))
        self$cost = cost
      if (!missing(gamma))
        self$gamma = gamma
      if (!missing(valid.times)) {
        self$valid.times = valid.times
        parent.obj = ParameterTuner$new(x = x, y = y, kernel = self$kernel, cost = self$cost, gamma = self$gamma, valid.times = self$valid.times)
      } else {
        parent.obj = ParameterTuner$new(x = x, y = y, kernel = self$kernel, cost = self$cost, gamma = self$gamma)
      }
      self$x = parent.obj$x
      self$y = parent.obj$y
      self$test.folds = parent.obj$test.folds
      self$cost = parent.obj$cost
      self$gamma = parent.obj$gamma
      if (!missing(file.prefix))
        self$file.prefix = file.prefix
      if (!missing(kfold.nb))
        self$kfold.nb = kfold.nb
      if (!missing(numcores))
        self$numcores = numcores
      if (.Platform$OS.type == "windows") self$numcores = 1
      if (!missing(halve.above))
        self$halve.above = halve.above
      #
      feature.ranking.obj <-
        FeatureRanking$new(
          x, y, valid.times = self$valid.times, kfold.nb = self$kfold.nb, halve.above =
            self$halve.above, numcores = self$numcores, file.prefix = file.prefix, cost=self$cost, gamma=self$gamma, kernel=self$kernel
        )
      self$feature.ranking = feature.ranking.obj$feature.ranking
      #
      if (!missing(feature.nb.vector))
        self$feature.nb.vector = feature.nb.vector
      #
      feature.nb.tuner.obj <-
        FeatureNbTuner$new(
          x = self$x, y = self$y, valid.times = self$valid.times, numcores = self$numcores, feature.ranking =
            self$feature.ranking, feature.nb.vector = self$feature.nb.vector, file.prefix =
            self$file.prefix, cost=self$cost, gamma=self$gamma, kernel=self$kernel
        )
      self$feature.performances = feature.nb.tuner.obj$feature.performances
      self$best.feature.nb = feature.nb.tuner.obj$best.feature.nb
      #
      model.perf.obj = ModelPerformance$new(
        x = x, y = y, feature.ranking = self$feature.ranking, feature.nb = self$best.feature.nb, file.prefix =
          self$file.prefix, cost=self$cost, gamma=self$gamma, kernel=self$kernel
      )
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


