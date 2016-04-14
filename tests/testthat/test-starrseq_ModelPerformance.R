test_that("starrseq_ModelPerformance", {

crms = read.table('data_starrseq/crm_features.tab')
y = crms[,1]
x = crms[,-1]

# -------------------------------------
valid.times=5
cost=1

# --------------------------
feature.ranking = read.table('data_starrseq/_feature_ranking.txt', header = T)

best.feature.nb=200
selected.features = as.character(feature.ranking$FeatureName[1:best.feature.nb])
x = x[,selected.features]

obj <- ModelPerformance$new(x = x, y = y, cost=cost)
testthat::expect_equal(as.numeric(obj$cv.probs.labels$probs[[2]]['mm9_chr18_3932278_3932479_+']), 0.0233, tolerance=1e-2)

cv.probs.labels <- evaluateModelPerformance(data=crms, cl=1, cost=cost)
testthat::expect_equal(as.numeric(cv.probs.labels$probs[[2]]['mm9_chr13_44305611_44305794_+']), 0.08956137, tolerance=1e-2)
}
)

