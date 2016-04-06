x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))

numcores=1

feature.nb.vector = list(2,4,6,8,10,12)

# -------------------------------------

test_that("iris2_LedPred", {

obj <- LedPred$new(x = x, y = y, numcores=numcores, feature.nb.vector=feature.nb.vector)
scores = obj$model.obj$ScoreData(x=x)$scores

testthat::expect_true(all(obj$feature.ranking[c(1,2), 'FeatureName']==c('Petal.Width', 'Petal.Length')))
testthat::expect_true(obj$best.feature.nb==4)
testthat::expect_true(all(rownames(obj$model$SV)[1:2]==c("1", "2")))
testthat::expect_equal(as.numeric(scores)[1], -0.533, tolerance=1e-2)
testthat::expect_true(all(obj$feature.ranking[c(11,12), 'AvgRank']==c(10.4, 11.4)))

})

test_that("iris2_LedPred_wrapper", {

obj <- ledpred(x = x, y = y, numcores=numcores, feature.nb.vector=feature.nb.vector)
scores = scoreData(x=x, ledpred=obj)

testthat::expect_true(all(obj$feature.ranking[c(1,2), 'FeatureName']==c('Petal.Width', 'Petal.Length')))
testthat::expect_true(all(obj$feature.ranking[c(11,12), 'AvgRank']==c(10.4, 11.4)))
testthat::expect_true(obj$feature.nb==4)
testthat::expect_true(all(rownames(obj$model$SV)[1:2]==c("1", "2")))

testthat::expect_equal(as.numeric(scores)[1], 0.998, tolerance=1e-2)

})


