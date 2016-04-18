x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))

# -------------------------------------
valid.times=5
numcores <- ifelse(.Platform$OS.type != "windows", 1,  parallel::detectCores() - 1)
cost=1

test_that("iris2_FeatureRanking_kfold.nb=1", {

kfold.nb=1

# -------------------------------------
feature.ranking.obj <-FeatureRanking$new(x = x, y = y, valid.times = valid.times, numcores=numcores, cost=cost)
testthat::expect_true(all(feature.ranking.obj$feature.ranking[c(1,2), 'FeatureName']==c('Petal.Width', 'Petal.Length')))
testthat::expect_true(all(feature.ranking.obj$feature.ranking[c(11,12), 'AvgRnk']==c(8.8, 1.6)))

})

test_that("iris2_rankFeatures_kfold.nb=1", {

kfold.nb=1

data=get(load(file="data_iris2/iris2.rda"))
cl=1

# -------------------------------------
feature.ranking <-rankFeatures(data = data, cl =cl , valid.times = valid.times, numcores=numcores, cost=cost)
#print(feature.ranking)
testthat::expect_true(all(feature.ranking[c(1,2), 'FeatureName']==c('Petal.Width', 'Petal.Length')))
testthat::expect_true(all(feature.ranking[c(11,12), 'AvgRnk']==c(9.8, 10)))

})

