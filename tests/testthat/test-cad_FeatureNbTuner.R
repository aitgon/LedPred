test_that("cad_FeatureNbTuner", {

crms = read.table('data_cad/crm_features_noscale.tab',sep="\t",header=TRUE,row.names=1)
y = crms[,1]
x = crms[,-1]
feature.ranking.obj = get(load('data_cad/feature.ranking.obj.rda'))
feature.nb.vector = list(70, 90, 110)

# -------------------------------------
valid.times=5

# -------------------------------------
feature.ranking = feature.ranking.obj$feature.ranking
feature.nb.tuner = FeatureNbTuner$new( x = x, y = y, valid.times = valid.times, feature.ranking = feature.ranking, feature.nb.vector = feature.nb.vector)

testthat::expect_true(feature.nb.tuner$best.feature.nb==90)
testthat::expect_equal(feature.nb.tuner$feature.performances['cv.kappa.mean',][[1]], 0.4598582, tolerance=1e-6)
}
)

