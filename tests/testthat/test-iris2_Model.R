x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))

# -------------------------------------
valid.times=5

test_that("iris2_Model", {
model.obj = Model$new( x = x, y = y)

#browser()

scores = model.obj$ScoreData(x=x)$scores
model = model.obj$model

testthat::expect_true(all(rownames(model$SV)[1:3]==c("21", "25", "35")))
testthat::expect_equal(as.numeric(scores)[1], 1.3936184, tolerance=1e-6)
testthat::expect_equal(as.numeric(scoreData(x, model.obj))[1], 1.393618, tolerance=1e-5)
}
)

