data(feature.ranking)
	cl=1
	kernel="linear"
	cost=1
	gamma=1
	scale=F
valid.times = 5
	feature.nb=70

data(svm.model)	
test_that("evaluate_model_performance (data.frame)", {

	data(feature.matrix)
	
probs.label.list <- evaluateModelPerformance(data=feature.matrix, cl=1, valid.times=valid.times, feature.rank=feature.ranking, feature.nb=feature.nb, kernel = kernel, scale = scale, cost = cost, gamma = gamma)
testthat::expect_equal(probs.label.list[[1]]$probs[[1]], 0.732, tolerance=1e-3)

})	

test_that("evaluate_model_performance (data.frame)", {

	data(crm.features)
	
probs.label.list <- evaluateModelPerformance(data.granges=crm.features, cl=1, valid.times=valid.times, feature.rank=feature.ranking, feature.nb=feature.nb, kernel = kernel, scale = scale, cost = cost, gamma = gamma)
testthat::expect_equal(probs.label.list[[1]]$probs[[1]], 0.732, tolerance=1e-3)


})

