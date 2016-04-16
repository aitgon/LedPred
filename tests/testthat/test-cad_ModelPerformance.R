test_that("cad_ModelPerformance", {

crms = read.table('data_cad/crm_features_noscale.tab',sep="\t",header=TRUE,row.names=1)
y = crms[,1]
x = crms[,-1]
feature.ranking.obj = get(load('data_cad/feature.ranking.obj.rda'))
best.feature.nb=90
cost=1 

# -------------------------------------
valid.times=5

# --------------------------

feature.ranking = feature.ranking.obj$feature.ranking
selected.features = as.character(feature.ranking$FeatureName[1:best.feature.nb])
x = x[,selected.features]

modelperf.obj <- ModelPerformance$new(x = x, y = y, valid.times=valid.times, cost=cost)

testthat::expect_equal(modelperf.obj$auc, 0.9, tolerance=1e-2)
testthat::expect_equal(as.numeric(modelperf.obj$cv.probs.labels$probs[[5]][1]), 0.2372886, tolerance=1e-1)

cv.probs.labels <- evaluateModelPerformance(data = crms, cl = 1, valid.times=valid.times, cost=cost)

testthat::expect_equal(as.numeric(cv.probs.labels$probs[[5]][1]), 0.0919, tolerance=1e-3)
}
)
