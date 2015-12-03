scoreData <-
  function(data = NULL, model, score.file = NULL) {
    message("scoreData is running ...")

    library(e1071)
    pred <-
      stats::predict(model, data, decision.values = TRUE, probability = TRUE)
    fit <- as.data.frame(attr(pred,"decision.values"))
    colnames(fit) <- c("score")
    fit = fit[order(-fit$score), , drop = FALSE]
    write.table(fit, file = score.file, col.names = FALSE, quote = FALSE)
    return(fit)
  }
