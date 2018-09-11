rmse <- function(y, yhat)
{
  error <- y - yhat
  sqrt(mean(error^2))
}

mase <- function(y, yhat) {
  in_sample_mae <-  sum(abs(diff(y))) / (length(y)-1)
  sum(abs(y - yhat)) / (length(y)*in_sample_mae)
}

mae <- function(y, yhat)
{
  error <- y - yhat
  mean(abs(error))
}

mape <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}