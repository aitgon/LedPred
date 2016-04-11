rankFeatures = function(x, y, ...) {
	feature.ranking.obj <- FeatureRanking$new(x = x, y = y, ...)
	feature.ranking = feature.ranking.obj$feature.ranking
	return(feature.ranking)
}

