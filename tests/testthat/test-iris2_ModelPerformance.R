x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))

data=get(load(file="data_iris2/iris2.rda"))
cl=1
# -------------------------------------

cost=1

test_that("fast_ModelPerformance", {

feature.ranking = get(load('data_iris2/feature.ranking.rda'))
best.feature.nb=2
selected.features = as.character(feature.ranking$FeatureName[1:best.feature.nb])
x = x[,selected.features]
modelperf.obj <- ModelPerformance$new(x = x, y = y, file.prefix=NULL)

testthat::expect_equal(modelperf.obj$auc, 1, tolerance=1e-2)
testthat::expect_equal(as.numeric(modelperf.obj$cv.probs.labels$probs[1,1]), 0.9784439, tolerance=1e-5)

cv.probs.labels <- evaluateModelPerformance(data = data, cl = 1, cost=cost)
testthat::expect_equal(as.numeric(cv.probs.labels$probs[1,1]), 0.999, tolerance=1e-3)
}
)

test_that("fast_ModelPerformance", {

# --------------------------
feature.ranking = get(load('data_iris2/feature.ranking.rda'))
best.feature.nb=2
selected.features = as.character(feature.ranking$FeatureName[1:best.feature.nb])
x = x[,selected.features]
modelperf.obj <- ModelPerformance$new(x = x, y = y, cost=cost, file.prefix=NULL)

testthat::expect_equal(modelperf.obj$auc, 1, tolerance=1e-2)
testthat::expect_equal(as.numeric(modelperf.obj$cv.probs.labels$probs[1,1]), 0.999417, tolerance=1e-5)

cv.probs.labels <- evaluateModelPerformance(data = data, cl = 1, cost=cost, file.prefix=NULL)
testthat::expect_equal(as.numeric(cv.probs.labels$probs[1,1]), 0.999, tolerance=1e-3)
}
)

