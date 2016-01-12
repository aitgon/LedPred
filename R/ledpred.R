rankFeatures = function(x, y, ...) {
	feature.ranking.obj <- ledpred2::FeatureRanking$new(x = x, y = y, ...)
#print(feature.ranking.o)
	feature.ranking = feature.ranking.obj$feature.ranking
	return(feature.ranking)
}

tuneFeatureNb = function(x, y, feature.ranking, feature.nb.vector = NULL, step.nb=25, ...) {

feature.nb.vector = seq(from = 10, to = (ncol(x) - 1), by = step.nb)
#print(feature.nb.vector)
feature.nb.tuner = ledpred2::FeatureNbTuner$new(
  x = x, y = y, feature.ranking = feature.ranking, feature.nb.vector = feature.nb.vector, ...)
    return(feature.nb.tuner)
}

createModel = function(x, y, feature.ranking = NULL, feature.nb = NULL, file.prefix = NULL, ...) {

selected.features = as.character(feature.ranking$FeatureName[1:feature.nb])
x = x[,selected.features]
model.obj <- ledpred2::Model$new(x = x, y = y)
model = model.obj$model
save(model, file = paste(file.prefix,"_model.rda",sep = "")) # can save model for later use

return(model.obj)
}

scoreData <- function(x, model, score.file = NULL) {

scores = as.numeric(model$ScoreData(x=x)$scores)
return(scores)

}

