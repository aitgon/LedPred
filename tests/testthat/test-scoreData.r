	data(svm.model)

test_that("scoreData (data.frame)", {

	data(feature.matrix)

scores = scoreData(data=feature.matrix, model=svm.model)

testthat::expect_equal(scores[1, 1], 0.9928403, tolerance=1e-7)

})

test_that("scoreData (genomic.ranges)", {

	data(seq.features)

scores = scoreData(data.granges=seq.features, model=svm.model)

testthat::expect_equal(scores[1, 1], 0.9928403, tolerance=1e-7)

})

