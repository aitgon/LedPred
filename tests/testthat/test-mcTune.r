rm(list=ls(all=TRUE))

valid.times = 2  
kernel="radial"
cost_vector=c(30, 100)
gamma_vector=c(1, 3)
scale=F
	
test_that("Tune cost gamma (seq.features.tab)", {

	# Get crm_matrix
	data(feature.matrix)
		
	
	c.g.obj <- mcTune(data = feature.matrix, cl = 1, ranges = list(gamma = gamma_vector, cost = cost_vector), kernel=kernel, scale=scale, valid.times=valid.times)

	expect_equal(c.g.obj$best.parameters$cost, 30)
	expect_equal(c.g.obj$best.parameters$gamma, 1)
})

test_that("Tune cost gamma (seq.features.granges)", {

	# Get crm_matrix
	data(seq.features)

c.g.obj <- mcTune(data.granges = seq.features, cl = 1, ranges = list(gamma = gamma_vector, cost = cost_vector), kernel=kernel, scale=scale, valid.times=valid.times)

	expect_equal(c.g.obj$best.parameters$cost, 30)
	expect_equal(c.g.obj$best.parameters$gamma, 1)
})

