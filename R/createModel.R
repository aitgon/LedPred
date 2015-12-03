createModel <- function(data, cl = 1) {
model <-
  e1071::svm(
    cl ~ ., data = data, kernel = 'linear', cost = 1, scale = F, probability =
      TRUE, decision.values = TRUE
  )
return(model)
}
