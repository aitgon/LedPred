test_that("starrseq_FeatureNbTuner", {

crms = read.table('data_starrseq/crm_features.tab')
y = crms[,1]
x = crms[,-1]


# -------------------------------------
valid.times=5

# -------------------------------------
feature.ranking = read.table('data_starrseq/_feature_ranking.txt', header=TRUE)
feature.nb.vector = list(100, 200)
feature.nb.tuner = FeatureNbTuner$new( x = x, y = y, valid.times = valid.times, feature.ranking = feature.ranking, feature.nb.vector = feature.nb.vector)

testthat::expect_true(feature.nb.tuner$best.feature.nb==100)
testthat::expect_equal(feature.nb.tuner$feature.performances['cv.kappa.mean',][[1]], 0.267, tolerance=1e-2)
}
)

