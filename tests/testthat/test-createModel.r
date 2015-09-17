	cl=1
	kernel="radial"
	cost=80
	gamma=2   
	scale=F
	data(feature.ranking)
	feature.nb=100

# ----------------------------------
test_that("create_model (data.frame)", {

	data(feature.matrix)
	
model <- createModel(data=feature.matrix, cl=cl, kernel=kernel, scale=scale, cost=cost, gamma=gamma, feature.rank=feature.ranking, feature.nb=feature.nb)
testthat::expect_equal(model$SV[3,1], 0.01499247, tolerance=1e-6)
  
})

# ----------------------------------
test_that("create_model (genomic.ranges)", {

	data(crm.features)
	
model <- createModel(data.granges=crm.features, cl=cl, kernel=kernel, scale=scale, cost=cost, gamma=gamma, feature.rank=feature.ranking, feature.nb=feature.nb)
testthat::expect_equal(model$SV[3,1], 0.01499247, tolerance=1e-6)
  
})

