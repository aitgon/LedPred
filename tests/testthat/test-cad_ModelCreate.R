crms = read.table('data_cad/crm_features_noscale.tab',sep="\t",header=TRUE,row.names=1)
y = crms[,1]
x = crms[,-1]
feature.ranking.obj = get(load('data_cad/feature.ranking.obj.rda'))
feature.nb = 90
valid.times=5
cost=1

crms.test = read.table('data_cad/crm_features_noscale_test.tab',sep="\t",header=TRUE,row.names=1)
x.test = crms.test[,-1]

test_that("cad_Model", {
feature.ranking = feature.ranking.obj$feature.ranking
model.obj = Model$new(x = x, y = y, valid.times=valid.times, feature.ranking = feature.ranking, feature.nb=feature.nb, cost=cost)

probs = model.obj$ScoreData(x=x.test, scale=TRUE)$probs
testthat::expect_equal(as.numeric(probs)[1], 0.9935163, tolerance=1e-6)
testthat::expect_equal(as.numeric(probs)[2], 3.792489e-06, tolerance=1e-6)

probs=scoreData(x.test, model=model.obj)
testthat::expect_equal(as.numeric(probs)[1], 0.9935163, tolerance=1e-6)
testthat::expect_equal(as.numeric(probs)[2], 3.792489e-06, tolerance=1e-6)
})

