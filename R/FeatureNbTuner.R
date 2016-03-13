FeatureNbTuner <- R6::R6Class(
  "FeatureNbTuner",
  inherit = Data,
  public = list(
    x = NULL,
    y = NULL,
    test.folds = NULL,
#    scale.factors = NULL,
    scale.center = NULL,
    scale.scale = NULL,
    valid.times = 5,
	file.prefix = NULL,
    numcores = parallel::detectCores() - 1,
    feature.ranking = NULL,
    feature.nb.vector = NULL,
    feature.performances = NULL,
    best.feature.nb = NULL,
    initialize = function(x, y, valid.times=self$valid.times, numcores=self$numcores, feature.ranking=self$feature.ranking, feature.nb.vector=self$feature.nb.vector, file.prefix=self$file.prefix) {
      if (!missing(valid.times)) {
        self$valid.times = valid.times
        data.obj = Data$new(x = x, y = y, valid.times = valid.times)
      } else {
        data.obj = Data$new(x = x, y = y)
      }
      self$x = data.obj$x
      self$y = data.obj$y
#      browser()
      self$test.folds = data.obj$test.folds
      if (!missing(file.prefix)) self$file.prefix = file.prefix
      if (!missing(numcores)) self$numcores = numcores
      if (!missing(feature.nb.vector)) self$feature.nb.vector = feature.nb.vector
self$feature.ranking = feature.ranking
ranked.features= as.character(self$feature.ranking$FeatureName)
self$feature.performances = private$FeatureKappaPerformance(feature.nb.vector=self$feature.nb.vector, ranked.features=ranked.features, test.folds=self$test.folds, x=self$x, y=self$y)
kappa.vector=simplify2array(self$feature.performances["cv.kappa.mean",])
  self$best.feature.nb = self$feature.nb.vector[min(which(kappa.vector == max(kappa.vector)))][[1]]
if (!is.null(self$file.prefix))
write(self$best.feature.nb, file=paste(file.prefix, "_best.feature.nb.txt", sep=""))
    private$PlotKappa(feature_nb_obj, file.prefix)
    }
  ),
  private = list(
PlotKappa = function(feature_nb_obj, file.prefix) {
feature.nb.vector=simplify2array(self$feature.nb.vector)
  cv.kappa.mean = simplify2array(self$feature.performances["cv.kappa.mean",])
  cv.kappa.sd = simplify2array(self$feature.performances["cv.kappa.sd",])
  best.feature.nb = self$best.feature.nb
png("")
ggplot2::qplot(feature.nb.vector,cv.kappa.mean) + ggplot2::geom_errorbar(ggplot2::aes(x=feature.nb.vector, ymin=cv.kappa.mean-cv.kappa.sd, ymax=cv.kappa.mean+cv.kappa.sd), width=0.25) + ggplot2::geom_line()
ggplot2::ggsave(paste(file.prefix, "_kappa_measures.png", sep=""));
garb = dev.off()
},
    FeatureKappaPerformance = function(feature.nb.vector, ranked.features, test.folds, x, y) {
      feature.performances = simplify2array(
        parallel::mclapply(feature.nb.vector, function(feature.nb) {
          x.features = x[, ranked.features[1:feature.nb]];  private$CalcKappaPerformanceAllFoldMean(test.folds =
                                                                                                              test.folds, x = x.features, y = y)
        }, mc.cores=self$numcores)
      )
	return(feature.performances)
    },
    CalcKappaPerformanceAllFoldMean = function(test.folds, x, y) {
#print(CalcKappaPerformanceOneFold(obj$test.folds[[1]], x=x, y=y))
  cv.kappa = simplify2array(parallel::mclapply(test.folds, function(test.fold.i) {
    private$CalcKappaPerformanceOneFold(x = x, y = y, test.fold.i = test.fold.i)
  }, mc.cores=self$numcores))
#print(cv.kappa)
  cv.kappa.mean = mean(cv.kappa);
  cv.kappa.sd = sd(cv.kappa);
  return(list(cv.kappa.mean = cv.kappa.mean, cv.kappa.sd = cv.kappa.sd));
},
CalcKappaPerformanceOneFold = function(test.fold.i, x, y) {
  test.set.x = x[test.fold.i,]
  test.set.y = y[test.fold.i]
  train.set.x = x[-test.fold.i,]
  train.set.y = y[-test.fold.i]
#browser()
  obj = Model$new(x = train.set.x, y = train.set.y, valid.times = 1)
  kappa = obj$CalcPredictionKappa(x = test.set.x, y = test.set.y)
  return(kappa)
}
  )
)

## -------------------------------------
#crms = read.table('data2/crm_features.tab')
#feature.ranking = read.table('data2/_feature_ranking.txt', header = T)
#ranked.features = feature.ranking$FeatureName

#crm.y = crms[,1]
#crm.x = crms[,-1]
#crm.x = crms[,ranked.features[1:515]]
#obj <- Data$new(x=crm.x, y=crm.y, valid.times=5)

