data(feature.ranking)
valid.times = 10
kernel="radial"
cost=80
gamma=2    
scale=F
step.nb=50

test_that("Tune feature nb (df)", {

print("Takes around 39 seconds in a 4 cored machine with 'Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz'")

data(feature.matrix)

tuned.feature.nb <- tuneFeatureNb(data = feature.matrix,cl = 1, feature.rank =feature.ranking, kernel=kernel, valid.times=valid.times, cost=cost, gamma=gamma, scale=scale, step.nb=step.nb)
expect_true(tuned.feature.nb$best.feature.nb==60)

})

test_that("Tune feature nb (granges)", {

print("Takes around 39 seconds in a 4 cored machine with 'Intel(R) Core(TM) i7-3520M CPU @ 2.90GHz'")

data(seq.features)

tuned.feature.nb <- tuneFeatureNb(data.granges = seq.features,cl = 1, feature.rank =feature.ranking, kernel=kernel, valid.times=valid.times, cost=cost, gamma=gamma, scale=scale, step.nb=step.nb)
expect_true(tuned.feature.nb$best.feature.nb==60)

})



