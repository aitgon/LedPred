x=get(load(file="data_iris2/x.rda"))
y=get(load(file="data_iris2/y.rda"))

# -------------------------------------
valid.times=5
numcores <- ifelse(.Platform$OS.type != "windows", 1,  parallel::detectCores() - 1)

test_that("iris2_Model", {

data.obj = Data$new(x=x, y=y)

testthat::expect_equal(as.numeric(data.obj$x[1,1]), 0.0706, tolerance=1e-3)
})


