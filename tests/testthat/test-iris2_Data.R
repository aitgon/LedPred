x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))

# -------------------------------------
valid.times=5
numcores=1

test_that("iris2_Model", {

data.obj = Data$new(x=x, y=y)
testthat::expect_equal(data.obj$x[1], -0.898, tolerance=1e-2)
testthat::expect_equal(data.obj$scale.center[[1]], 5.843333, tolerance=1e-7)
})


