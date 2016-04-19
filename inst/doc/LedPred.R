### R code from vignette source 'LedPred.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: crm.features
###################################################
library(LedPred)
data(crm.features)


###################################################
### code chunk number 2: mapFeaturesToCRMs
###################################################
dirPath <- system.file("extdata", package="LedPred")
file.list <-   list.files(dirPath, full.names=TRUE)
background.freq <- file.list[grep("freq", file.list)]
positive.regions <-  file.list[grep("positive", file.list)]
negative.regions <-  file.list[grep("negative", file.list)]
TF.matrices <-  file.list[grep("tf", file.list)]
ngs.path <- system.file("extdata/ngs", package="LedPred")
ngs.files=list.files(ngs.path, full.names=TRUE)
#crm.feature.list <- mapFeaturesToCRMs(URL = 'http://ifbprod.aitorgonzalezlab.org/map_features_to_crms.php', positive.bed=positive.regions, negative.bed=negative.regions, background.freqs=background.freq, pssm=TF.matrices, genome="dm3", ngs=ngs.files,  crm.feature.file = "vignette_crm.features.tab",  stderr.log.file = "vignette_stderr.log", stdout.log.file = "vignette_stdout.log")
#names(crm.feature.list)
#class(crm.feature.list$crm.features)


###################################################
### code chunk number 3: mcTune
###################################################
#crm.features=crm.feature.list$crm.features
#cost.vector <- c(1,3,10)
#gamma.vector <- c(1,3,10)
#c.g.obj <- mcTune(data = crm.features, ranges = list(cost=cost.vector, gamma=gamma.vector), kernel='linear', file.prefix = "vignette")
#names(c.g.obj)
#cost <- c.g.obj$e1071.tune.obj$best.parameters$cost
#gamma <- c.g.obj$e1071.tune.obj$best.parameters$gamma


###################################################
### code chunk number 4: LedPred.Rnw:170-172
###################################################
#feature.ranking <- rankFeatures(data=crm.features, cost=cost, gamma=gamma, kernel='linear', file.prefix = "vignette")
#head(feature.ranking)


###################################################
### code chunk number 5: LedPred.Rnw:178-181
###################################################
#feature.nb.tuner.obj <- tuneFeatureNb(data=crm.features, feature.ranking=feature.ranking, kernel='linear', cost=cost, gamma=gamma, file.prefix = "vignette")
#names(feature.nb.tuner.obj)
#feature.nb.tuner.obj$best.feature.nb


###################################################
### code chunk number 6: LedPred.Rnw:193-197
###################################################
#feature.nb <- 60
#model.obj <- createModel(data=crm.features, cost=cost, gamma=gamma, feature.ranking=feature.ranking, feature.nb=feature.nb, file.prefix="vignette")
#names(model.obj)
#model.obj$model


###################################################
### code chunk number 7: LedPred.Rnw:202-204
###################################################
#feature.weights <- as.data.frame(t(t(model.obj$model$coefs) %*% model.obj$model$SV))
#head(feature.weights)


###################################################
### code chunk number 8: LedPred.Rnw:210-211
###################################################
#probs.labels.list <- evaluateModelPerformance(data=crm.features, feature.ranking=feature.ranking, feature.nb=feature.nb, cost=cost, gamma=gamma, file.prefix = "vignette")


###################################################
### code chunk number 9: mapFeaturesToCRMs2
###################################################
dirPath <- system.file("extdata", package="LedPred")
file.list <-   list.files(dirPath, full.names=TRUE)
background.freqs <- file.list[grep("freq", file.list)]
positive.bed <-  file.list[grep("prediction_small", file.list)]
TF.matrices <-  file.list[grep("259_matrices_lightNames", file.list)]
ngs.path <- system.file("extdata/ngs", package="LedPred")
ngs.files=list.files(ngs.path, full.names=TRUE)
#prediction.crm.features.list <- mapFeaturesToCRMs(URL = 'http://ifbprod.aitorgonzalezlab.org/map_features_to_crms.php', positive.bed=positive.bed, 
#  pssm=TF.matrices, background.freqs=background.freqs, 
#  genome='dm3', ngs=ngs.files, feature.ranking=feature.ranking, feature.nb=feature.nb, 
#    crm.feature.file = "vignette_pred.crm.features.tab", 
#    stderr.log.file = "vignette_pred.stderr.log", stdout.log.file = "vignette_pred.stdout.log")
#names(prediction.crm.features.list)
#prediction.crm.features <- prediction.crm.features.list$crm.features


###################################################
### code chunk number 10: scoreData
###################################################
#pred.test <- scoreData(data=prediction.crm.features, model=model.obj, score.file="vignette_prediction.tab")
#pred.test


###################################################
### code chunk number 11: ledpred
###################################################
crm.features=data(crm.features)
cost.vector <- c(1,3,10)
gamma.vector <- c(1,3,10)
#ledpred.obj=LedPred(data=crm.features, cl=1, ranges = list(cost=cost.vector, gamma=gamma.vector), kernel="linear", file.prefix="vignette2")
#names(ledpred.obj)


###################################################
### code chunk number 12: mapFeaturesToCRMs3
###################################################
dirPath <- system.file("extdata", package="LedPred")
file.list <-   list.files(dirPath, full.names=TRUE)
background.freqs <- file.list[grep("freq", file.list)]
positive.bed <-  file.list[grep("prediction_small", file.list)]
TF.matrices <-  file.list[grep("259_matrices_lightNames", file.list)]
ngs.path <- system.file("extdata/ngs", package="LedPred")
ngs.files=list.files(ngs.path, full.names=TRUE)
#prediction.crm.features.obj2 <- mapFeaturesToCRMs(URL = 'http://ifbprod.aitorgonzalezlab.org/map_features_to_crms.php', positive.bed=positive.bed, 
#  pssm=TF.matrices, background.freqs=background.freqs, 
#  genome='dm3', ngs=ngs.files, feature.ranking=ledpred.obj$feature.ranking, feature.nb=ledpred.obj$feature.nb, 
#    crm.feature.file = "vignette2_pred.crm.features.tab", 
#    stderr.log.file = "vignette2_pred.stderr.log", stdout.log.file = "vignette2_pred.stdout.log")
#names(prediction.crm.features.obj2)
#prediction.crm.features2 <- prediction.crm.features.obj2$crm.features


###################################################
### code chunk number 13: scoreData2 (eval = FALSE)
###################################################
## #pred.test2 <- scoreData(data=prediction.crm.features2, model=ledpred.list$model.obj, score.file="vignette2_prediction.tab")
## #pred.test2


###################################################
### code chunk number 14: LedPred.Rnw:290-291
###################################################
sessionInfo()


