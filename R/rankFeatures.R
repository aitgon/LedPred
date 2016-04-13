rankFeatures = function(data, cl=1, ...) {


x=data[,-cl]
y=data[,cl]

	feature.ranking.obj <- FeatureRanking$new(x = x, y = y, ...)
	feature.ranking = feature.ranking.obj$feature.ranking
	return(feature.ranking)
}

