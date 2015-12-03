test_that(".makeCrossValidSets", {
  load("mat.Rda")
  testi_bak <- get(load("testi.Rda"))

  testi.vector <- .makeCrossValidSets(mat, 5)
  testthat::expect_true(all(testi.vector[[1]]==testi_bak))
})
