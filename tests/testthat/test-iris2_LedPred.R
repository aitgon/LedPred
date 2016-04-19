x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))

feature.nb.vector = list(2,4,6,8,10,12)
step.nb=2
cost=1

# -------------------------------------

test_that("iris2_LedPredClass", {

obj <- LedPredClass$new(x = x, y = y, feature.nb.vector=feature.nb.vector, cost=cost)
scores = obj$model.obj$ScoreData(x=x)$scores

testthat::expect_true(all(obj$feature.ranking[c(1,2), 'FeatureName']==c('Petal.Width', 'Petal.Length')))
testthat::expect_true(obj$best.feature.nb==4)
testthat::expect_true(all(rownames(obj$model$SV)[1:2]==c("1", "2")))
testthat::expect_equal(as.numeric(scores)[1], -0.533, tolerance=1e-2)
testthat::expect_true(all(obj$feature.ranking[c(11,12), 'AvgRank']==c(10.4, 11.4)))

})

test_that("iris2_LedPred", {

data=get(load(file="data_iris2/iris2.rda"))
cl=1

obj <- LedPred(data = data, cl = cl, step.nb=step.nb, cost=cost)
scores = scoreData(data=x, ledpred=obj)

testthat::expect_true(all(obj$feature.ranking[c(1,2), 'FeatureName']==c('Petal.Width', 'Petal.Length')))
testthat::expect_true(all(obj$feature.ranking[c(11,12), 'AvgRank']==c(10.8, 11.4)))
testthat::expect_true(obj$feature.nb==4)
testthat::expect_true(all(rownames(obj$model$SV)[1:2]==c("1", "2")))

testthat::expect_equal(as.numeric(scores)[1], 0.998, tolerance=1e-2)

})


