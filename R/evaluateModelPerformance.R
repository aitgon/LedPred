

.calculatePredictionProbability = function(testi, data = dat, cl = 1, kernel = "radial", scale = FALSE, cost = NULL, gamma = NULL, feature.ranking =
                                            NULL, feature.nb = NULL) {
  trainset = data[-testi,]
  testset = data[testi,]

  classfit <-
    e1071::svm(
      cl ~ ., data = trainset, kernel = 'linear', cost = 1, scale = F, probability =
        TRUE, decision.values = TRUE
    )
  library(e1071)
  classpred = predict(classfit, testset[,-1], decision.values = T, probability = TRUE)
  probs = attr(classpred,"decision.values")[,1]
  labels = testset$cl
  return (list(probs = probs, labels = labels))
}
