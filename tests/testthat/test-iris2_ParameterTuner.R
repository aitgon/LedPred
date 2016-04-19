kernel='linear'
type='C-classification'
ranges = list(gamma = 2^(-1:1), cost = 2^(2:4))
valid.times = 10
numcores <- ifelse(.Platform$OS.type == "windows", 1,  parallel::detectCores() - 1)
#numcores = 1

test_that("iris2_ParameterTuner", {

x = iris[,-which(names(iris) == "Species")]
y = ifelse(iris$Species=='setosa',1,-1)

e1071.tune <- e1071::tune(e1071::svm, train.y=as.factor(y), train.x=x, kernel=kernel, type=type, ranges = ranges, tunecontrol = e1071::tune.control(sampling = "cross", cross=valid.times))

tune.parameters.obj <-ParameterTuner$new(x = x, y = y, kernel=kernel, ranges=ranges, valid.times = valid.times, numcores=numcores)

testthat::expect_true(tune.parameters.obj$cost==e1071.tune$best.parameters$cost)
testthat::expect_true(tune.parameters.obj$gamma==e1071.tune$best.parameters$gamma)

})

test_that("mtcars_ParameterTuner", {

data(mtcars)
x = mtcars[,-which(names(mtcars) == "vs")]
y = ifelse(mtcars$vs==1,1,-1)

e1071.tune <- e1071::tune(e1071::svm, train.y=as.factor(y), train.x=x, kernel=kernel, type=type, ranges = ranges, tunecontrol = e1071::tune.control(sampling = "cross", cross=valid.times))

testthat::expect_true(e1071.tune$best.parameters$cost==4)
testthat::expect_true(e1071.tune$best.parameters$gamma==0.5)

tune.parameters.obj <-ParameterTuner$new(x = x, y = y, kernel=kernel, ranges=ranges, valid.times = valid.times, numcores=numcores)

testthat::expect_true(tune.parameters.obj$cost==16)
testthat::expect_true(tune.parameters.obj$gamma==0.5)

})

