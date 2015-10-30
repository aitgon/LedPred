# In construction

ledpred.obj <- LedPred::LedPred(data.granges = crm.features, cl = 1, valid.times = 5, numcores = parallel::detectCores() - 1, kernel = "linear", scale = F, step.nb=40)


