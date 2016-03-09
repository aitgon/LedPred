rankFeatures = function(x, y, ...) {
	feature.ranking.obj <- ledpred2::FeatureRanking$new(x = x, y = y, ...)
	feature.ranking = feature.ranking.obj$feature.ranking
	return(feature.ranking)
}

