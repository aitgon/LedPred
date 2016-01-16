LedPred <- R6::R6Class(
  "LedPred",
  inherit = Data,
  public = list(
    x = NULL,
    y = NULL,
    valid.times = 5,
    scale.factors = NULL,
    kfold.nb = 1,
    halve.above = 100,
    numcores = parallel::detectCores() - 1,
    feature.ranking = NULL,
    file.prefix = NULL,
    feature.nb.vector = NULL,
    feature.performances = NULL,
    best.feature.nb = NULL,
    model = NULL,
    model.obj = NULL,
    weights = NULL,
    cv.probs.labels = NULL,
    initialize = function(x, y, valid.times=self$valid.times, kfold.nb=self$kfold.nb, halve.above=self$halve.above, numcores=self$numcores, file.prefix=self$file.prefix, feature.nb.vector) {
    #
    feature.ranking.obj <- ledpred2::FeatureRanking$new(x, y, valid.times=self$valid.times, kfold.nb=self$kfold.nb, halve.above=self$halve.above, numcores=self$numcores, file.prefix=file.prefix)
    #
    self$x = feature.ranking.obj$x
    self$y = feature.ranking.obj$y
    self$valid.times = feature.ranking.obj$valid.times
    self$test.folds = feature.ranking.obj$test.folds
    self$scale.factors = feature.ranking.obj$scale.factors
    self$file.prefix = feature.ranking.obj$file.prefix
    self$numcores = feature.ranking.obj$numcores
    self$feature.ranking = feature.ranking.obj$feature.ranking
    #
    self$feature.nb.vector = feature.nb.vector
    #
    feature.nb.tuner.obj <- ledpred2::FeatureNbTuner$new(x=self$x, y=self$y, valid.times=self$valid.times, numcores=self$numcores, feature.ranking=self$feature.ranking, feature.nb.vector=self$feature.nb.vector, file.prefix=self$file.prefix)
    #    
    self$feature.performances = feature.nb.tuner.obj$feature.performances
    self$best.feature.nb = feature.nb.tuner.obj$best.feature.nb
    #
    model.perf.obj = ModelPerformance$new(x = x, y = y, feature.ranking=self$feature.ranking, feature.nb=self$best.feature.nb, file.prefix=self$file.prefix)
##    #
    self$model = model.perf.obj$model
    self$model.obj = model.perf.obj$model.obj
    self$weights = model.perf.obj$weights
    self$cv.probs.labels = model.perf.obj$cv.probs.labels
    #
#    print(self$feature.ranking)
#    print(self$best.feature.nb)
#    print(rownames(self$model$SV)[1:3])
#    print(self$cv.probs.labels$probs[1,1])
    }
    )
)
