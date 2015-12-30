test_that("fast_FeatureRanking", {

x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))

# -------------------------------------
valid.times=5
kfold.nb=5
numcores=1

# -------------------------------------
feature.ranking.obj <-FeatureRanking$new(x = x, y = y, valid.times = valid.times, kfold.nb=kfold.nb)
testthat::expect_true(all(feature.ranking.obj$feature.ranking[c(1,2), 'FeatureName']==c('Petal.Length', 'Petal.Width')))
})

