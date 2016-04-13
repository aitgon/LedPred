crms = read.table('data_cad/crm_features_noscale.tab',sep="\t",header=TRUE,row.names=1)
y = crms[,1]
x = crms[,-1]
valid.times = 2
kernel="radial"
cost.vector <- c(1, 3, 10) # best 3
gamma.vector <- c(1, 3, 10) # best 1
numcores = parallel::detectCores() - 1

# -------------------------------------

test_that("ParameterTuner", {


tune.parameters.obj <-ParameterTuner$new(x = x, y = y, kernel=kernel, ranges=list(cost=cost.vector, gamma=gamma.vector), valid.times = valid.times, numcores=numcores)

testthat::expect_true(tune.parameters.obj$cost==3)
testthat::expect_true(tune.parameters.obj$gamma==3)
})

test_that("mcTune", {


tune.parameters.obj <-mcTune(data = crms, cl = 1, kernel=kernel, ranges=list(cost=cost.vector, gamma=gamma.vector), valid.times = valid.times, numcores=numcores)

testthat::expect_true(tune.parameters.obj$cost==3)
testthat::expect_true(tune.parameters.obj$gamma==3)
})


