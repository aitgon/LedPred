rankFeatures = function(x, y, ...) {
	feature.ranking.obj <- ledpred2::FeatureRanking$new(x = x, y = y, ...)
	feature.ranking = feature.ranking.obj$feature.ranking
	return(feature.ranking)
}

tuneFeatureNb = function(x, y, feature.ranking, feature.nb.vector = NULL, step.nb=25, ...) {

feature.nb.vector = seq(from = 10, to = (ncol(x) - 1), by = step.nb)
feature.nb.tuner = ledpred2::FeatureNbTuner$new(
  x = x, y = y, feature.ranking = feature.ranking, feature.nb.vector = feature.nb.vector, ...)
    return(feature.nb.tuner)
}

createModel = function(x, y, feature.ranking = NULL, feature.nb = NULL, file.prefix = NULL, ...) {

#selected.features = as.character(feature.ranking$FeatureName[1:feature.nb])
#x = x[,selected.features]
model.obj <- ledpred2::Model$new(x = x, y = y, file.prefix=file.prefix, feature.ranking = feature.ranking, feature.nb = feature.nb)
#save(model.obj, file = paste(file.prefix,"_model.rda",sep = "")) # can save model for later use

return(model.obj)
}

scoreData <- function(x, ledpred=NULL, model=NULL, score.file = NULL) {

scores=NULL

if(!is.null(ledpred))  {
	scores = ledpred$model.obj$ScoreData(x=x)$probs

} else if (!is.null(model))  {
	scores = model$ScoreData(x=x)$probs
}

if(!is.null(score.file)) {
	write.table(scores, file=score.file, quote=FALSE, col.names=FALSE)
}
return(scores)

}

evaluateModelPerformance = function(x, y, ...) {

cv.probs.labels = ModelPerformance$new(x, y, ...)$cv.probs.labels

return(cv.probs.labels)

}

ledpred = function(x, y, ...) {
obj <- LedPred$new(x = x, y = y, ...)
    ledpred.summary <-
      list(
        feature.ranking = obj$feature.ranking, feature.nb =
          obj$best.feature.nb, model.obj = obj$model.obj, test.folds=obj$test.folds, probs.label.list = obj$probs.label.list
      )
      return(ledpred.summary)
}

