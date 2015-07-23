kernel="linear"
cost=80
gamma=2    
valid.times=2
scale=F

test_that("Rank features (df)", {
  
data(feature.matrix)

feature.rank <- rankFeatures(data=feature.matrix, halve.above=100, valid.times=valid.times, kernel=kernel, cost=cost, gamma=gamma, scale=scale)

expect_true(all(feature.rank[1,1:3]== c("dm_Max_SANGER_10_FBgn0000472", 175, 7)))

})

test_that("Rank features (seq.ranges)", {
  
data(seq.features)

feature.rank <- rankFeatures(data.granges=seq.features, halve.above=100, valid.times=valid.times, kernel=kernel, cost=cost, gamma=gamma, scale=scale)

expect_true(all(feature.rank[1,1:3]== c("dm_Max_SANGER_10_FBgn0000472", 175, 7)))

})


