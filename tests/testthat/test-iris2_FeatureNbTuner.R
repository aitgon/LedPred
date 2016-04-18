x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))

# -------------------------------------
valid.times=5
numcores <- ifelse(.Platform$OS.type != "windows", 1,  parallel::detectCores() - 1)
cost=1

# -------------------------------------
feature.ranking = get(load('data_iris2/feature.ranking.rda'))
feature.nb.vector = list(2,4,6,8,10,12)

test_that("iris2_FeatureNbTuner_costIsNull", {
feature.nb.tuner = FeatureNbTuner$new( x = x, y = y, valid.times = valid.times, feature.ranking = feature.ranking, feature.nb.vector = feature.nb.vector, numcores=numcores)

testthat::expect_true(feature.nb.tuner$best.feature.nb==2)
testthat::expect_equal(feature.nb.tuner$feature.performances['cv.kappa.mean',][[1]], 1,tolerance=1e-6)
})

test_that("iris2_FeatureNbTuner", {
feature.nb.tuner = FeatureNbTuner$new( x = x, y = y, valid.times = valid.times, feature.ranking = feature.ranking, feature.nb.vector = feature.nb.vector, numcores=numcores, cost=cost)

testthat::expect_true(feature.nb.tuner$best.feature.nb==4)
testthat::expect_equal(feature.nb.tuner$feature.performances['cv.kappa.mean',][[1]], 0.9009967,tolerance=1e-6)
})

test_that("iris2_tuneFeatureNb", {

data=get(load(file="data_iris2/iris2.rda"))
cl=1
step.nb=2

feature.nb.tuner = tuneFeatureNb(data = data, cl = cl, step.nb=step.nb, valid.times = valid.times, feature.ranking = feature.ranking, numcores=numcores, cost=cost)

testthat::expect_true(feature.nb.tuner$best.feature.nb==2)
testthat::expect_equal(feature.nb.tuner$feature.performances['cv.kappa.mean',][[1]], 1,tolerance=1e-2)
})

