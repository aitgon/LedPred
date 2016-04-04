x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))
# -------------------------------------

test_that("iris2_Model", {
model.obj = Model$new( x = x, y = y)

scores = model.obj$ScoreData(x=x)$scores
model = model.obj$model

testthat::expect_true(all(rownames(model$SV)[1:3]==c("21", "25", "35")))
testthat::expect_equal(as.numeric(scores)[1], 1.392878, tolerance=1e-2)
testthat::expect_equal(as.numeric(scoreData(x, model=model.obj))[1], 0.9968594, tolerance=1e-2)
})

test_that("iris2_Model_Features", {

feature.ranking = get(load('data_iris2/feature.ranking.rda'))
feature.nb = 4

model.obj = Model$new(x = x, y = y, feature.ranking=feature.ranking, feature.nb=feature.nb)
scores = model.obj$ScoreData(x=x)$scores
model = model.obj$model

testthat::expect_true(all(rownames(model$SV)[1:3]==c("24", "42", "44")))
testthat::expect_equal(as.numeric(scores)[1], 1.455133, tolerance=1e-5)
testthat::expect_equal(as.numeric(scoreData(x, model=model.obj))[1], 0.9955539, tolerance=1e-6)

})

test_that("iris2_createModel", {

feature.ranking = get(load('data_iris2/feature.ranking.rda'))
feature.nb = 4

model.obj = createModel(x = x, y = y, feature.ranking=feature.ranking, feature.nb=feature.nb)
scores = model.obj$ScoreData(x=x)$scores
model = model.obj$model

testthat::expect_true(all(rownames(model$SV)[1:3]==c("24", "42", "44")))
testthat::expect_equal(as.numeric(scores)[1], 1.455133, tolerance=1e-1)
testthat::expect_equal(as.numeric(scoreData(x, model=model.obj))[1], 0.9955539, tolerance=1e-6)

})

