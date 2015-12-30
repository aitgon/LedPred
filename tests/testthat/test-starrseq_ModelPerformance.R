test_that("starrseq_ModelPerformance", {

crms = read.table('data_starrseq/crm_features.tab')
y = crms[,1]
x = crms[,-1]

# -------------------------------------
valid.times=5

# --------------------------
feature.ranking = read.table('data_starrseq/_feature_ranking.txt', header = T)

best.feature.nb=200
selected.features = as.character(feature.ranking$FeatureName[1:best.feature.nb])
x = x[,selected.features]
obj <- ModelPerformance$new(x = x, y = y)
#browser()
testthat::expect_equal(as.numeric(obj$cv.probs.labels$probs[[2]]['mm9_chr18_3932278_3932479_+']), 0.02596943, tolerance=1e-7)
}
)

