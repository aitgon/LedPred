x=get(load(file="data_TuneParameters/x.rda"))
y=get(load(file="data_TuneParameters/y.rda"))

# -------------------------------------
valid.times = 5
kernel="radial"
cost_vector=c(30, 100)
gamma_vector=c(1, 3)


test_that("TuneParameters", {

tune.parameters.obj <-TuneParameters$new(x = x, y = y, valid.times = valid.times, numcores=numcores)

})

