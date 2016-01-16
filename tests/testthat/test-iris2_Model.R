x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))

# -------------------------------------
valid.times=5

test_that("iris2_Model", {
model.obj = Model$new( x = x, y = y)

scores = model.obj$ScoreData(x=x)$scores
model = model.obj$model

#browser()

testthat::expect_true(all(rownames(model$SV)[1:3]==c("1", "2", "3")))
testthat::expect_equal(as.numeric(scores)[1], -0.737, tolerance=1e-2)
testthat::expect_equal(as.numeric(scoreData(x, model.obj))[1], -0.737, tolerance=1e-2)
}
)

