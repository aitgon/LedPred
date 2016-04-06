crms = read.table('data_cad/crm_features_noscale.tab',sep="\t",header=TRUE,row.names=1)
y = crms[,1]
x = crms[,-1]
feature.ranking.obj = get(load('data_cad/feature.ranking.obj.rda'))
feature.nb = 90
valid.times=5

test_that("cad_Model", {
feature.ranking = feature.ranking.obj$feature.ranking
model.obj = Model$new(x = x, y = y, valid.times=valid.times, feature.ranking = feature.ranking, feature.nb=feature.nb)
browser()

probs = model.obj$ScoreData(x=x)$probs

testthat::expect_equal(as.numeric(probs)[1], 0.2076358, tolerance=1e-6)
testthat::expect_equal(as.numeric(scoreData(x, model=model.obj))[1], 0.9999999, tolerance=1e-6)
})

