#' Perform common logistic regression calculations
#' @export
mylogistic = function(X, Y) {
  X = as.data.frame(X)  
  data = cbind(Y = Y, X)
  
  model = glm(Y ~ ., data = data, family = "binomial")
  Yhat = predict(model, type = "response")
  SSSE = sum((Y - Yhat)^2)
  RMSE = sqrt(mean((Y - Yhat)^2))
  bhat = coef(model)
  
  logi.predict = function(newX) {
    newX = as.data.frame(newX)
    predict(model, newdata = newX, type = "response")
  }
  
  return(list(
    bhat = bhat,
    Yhat = Yhat,
    SSE = SSSE,
    RMSE = RMSE,
    logi.predict = logi.predict
  ))
}