x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))
cost=1
# -------------------------------------

test_that("iris2_Model_noCost", {
model.obj = Model$new( x = x, y = y)

scores = model.obj$ScoreData(x=x)$scores
model = model.obj$model

testthat::expect_true(all(rownames(model$SV)[1:3]==c("2", "4", "6")))
testthat::expect_equal(as.numeric(scores)[1], 1.032471, tolerance=1e-2)
testthat::expect_equal(as.numeric(scoreData(x, model=model.obj))[1], 0.9912151, tolerance=1e-2)
})

test_that("iris2_Model", {
model.obj = Model$new( x = x, y = y, cost=cost)


scores = model.obj$ScoreData(x=x)$scores
model = model.obj$model

testthat::expect_true(all(rownames(model$SV)[1:3]==c("1", "2", "3")))
testthat::expect_equal(as.numeric(scores)[1], -0.534, tolerance=1e-2)
testthat::expect_equal(as.numeric(scoreData(x, model=model.obj))[1], 0.9968594, tolerance=1e-2)
})

test_that("iris2_Model_Features", {

feature.ranking = get(load('data_iris2/feature.ranking.rda'))
feature.nb = 4

model.obj = Model$new(x = x, y = y, feature.ranking=feature.ranking, feature.nb=feature.nb, cost=cost)
scores = model.obj$ScoreData(x=x)$scores
model = model.obj$model

testthat::expect_true(all(rownames(model$SV)[1:3]==c("1", "2", "3")))
testthat::expect_equal(as.numeric(scores)[1], -0.548, tolerance=1e-2)
testthat::expect_equal(as.numeric(scoreData(x, model=model.obj))[1], 1, tolerance=1e-3)

})

test_that("iris2_createModel", {

feature.ranking = get(load('data_iris2/feature.ranking.rda'))
feature.nb = 4

model.obj = createModel(x = x, y = y, feature.ranking=feature.ranking, feature.nb=feature.nb, cost=cost)
scores = model.obj$ScoreData(x=x)$scores
model = model.obj$model

testthat::expect_true(all(rownames(model$SV)[1:3]==c("1", "2", "3")))
testthat::expect_equal(as.numeric(scores)[1], -0.548, tolerance=1e-2)
testthat::expect_equal(as.numeric(scoreData(x, model=model.obj))[1], 1, tolerance=1e-3)

})

