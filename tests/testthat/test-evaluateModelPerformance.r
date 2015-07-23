data(feature.ranking)
	cl=1
	kernel="linear"	
	cost=1
	gamma=1   
	scale=F
valid.times = 2
	feature.nb=70

data(svm.model)	
test_that("evaluate_model_performance (data.frame)", {

	data(feature.matrix)
	
probs.label.list <- evaluateModelPerformance(data=feature.matrix, cl=1, valid.times=valid.times, svm.model=svm.model, feature.rank=feature.ranking, feature.nb=feature.nb)
testthat::expect_equal(probs.label.list[[1]]$probs[[1]], 0.8182657, tolerance=1e-6)

})	

test_that("evaluate_model_performance (data.frame)", {

	data(seq.features)
	
probs.label.list <- evaluateModelPerformance(data.granges=seq.features, cl=1, valid.times=valid.times, svm.model=svm.model, feature.rank=feature.ranking, feature.nb=feature.nb)
testthat::expect_equal(probs.label.list[[1]]$probs[[1]], 0.8182657, tolerance=1e-6)

})

