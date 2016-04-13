tuneFeatureNb = function(data, cl=1, feature.ranking, step.nb=10, ...) {

x=data[,-cl]
y=data[,cl]

feature.nb.vector = seq(from = step.nb, to = (ncol(x) - 1), by = step.nb)

feature.nb.tuner = FeatureNbTuner$new(
  x = x, y = y, feature.ranking = feature.ranking, feature.nb.vector = feature.nb.vector, ...)
    return(feature.nb.tuner)
}
