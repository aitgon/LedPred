tuneFeatureNb = function(x, y, feature.ranking, feature.nb.vector = seq(from = 10, to = (ncol(x) - 1), by = step.nb), step.nb=25, ...) {

feature.nb.tuner = ledpred2::FeatureNbTuner$new(
  x = x, y = y, feature.ranking = feature.ranking, feature.nb.vector = feature.nb.vector, ...)
    return(feature.nb.tuner)
}
