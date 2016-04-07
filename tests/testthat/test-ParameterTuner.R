# -------------------------------------


test_that("ParameterTuner", {

crms = read.table('data_cad/crm_features_noscale.tab',sep="\t",header=TRUE,row.names=1)
y = crms[,1]
x = crms[,-1]
valid.times = 2
kernel="radial"
cost.vector <- c(0.3, 1, 3) # best 3
gamma.vector <- c(0.3, 1, 3) # best 1
numcores=30


tune.parameters.obj <-ParameterTuner$new(x = x, y = y, kernel=kernel, ranges=list(cost=cost.vector, gamma=gamma.vector), valid.times = valid.times, numcores=numcores, file.prefix="_")

})

