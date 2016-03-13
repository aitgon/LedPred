x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))

# -------------------------------------
valid.times=5

# -------------------------------------
feature.ranking = get(load('data_iris2/feature.ranking.rda'))
feature.nb.vector = list(2,4,6,8,10,12)

test_that("iris2_FeatureNbTuner", {
feature.nb.tuner = tuneFeatureNb( x = x, y = y, valid.times = valid.times, feature.ranking = feature.ranking, feature.nb.vector = feature.nb.vector)
testthat::expect_true(feature.nb.tuner$best.feature.nb==10)
testthat::expect_equal(feature.nb.tuner$feature.performances['cv.kappa.mean',][[1]], 1,tolerance=1e-2)
}
)

