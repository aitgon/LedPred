x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))
# -------------------------------------

test_that("fast_ModelPerformance", {

# --------------------------
feature.ranking = get(load('data_iris2/feature.ranking.rda'))
best.feature.nb=2
selected.features = as.character(feature.ranking$FeatureName[1:best.feature.nb])
x = x[,selected.features]

obj <- ModelPerformance$new(x = x, y = y)
testthat::expect_equal(as.numeric(obj$cv.probs.labels$probs[1,1]), 0.316, tolerance=1e-2)

cv.probs.labels <- evaluateModelPerformance(x = x, y = y)
testthat::expect_equal(as.numeric(cv.probs.labels$probs[1,1]), 0.316, tolerance=1e-2)
}
)

