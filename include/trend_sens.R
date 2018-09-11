#Note that the Slope is in units / day. 
Trend_MK_Sens <- function (timestamp, usage) {
  #Verify timestamp, usage
  timestamp <- as.POSIXct(timestamp, origin='1970-01-01')
  
  if (length(unique(timestamp)) != length(timestamp)) {
    print("WARNING: Dataset has repeated datapoints in timestamp");
  }
  if (all(usage == usage[1])) {
    print("WARNING: Dataset has repeated datapoints in usage");
    return(list("trend"=NA,"intercept"=NA,"slope"=NA,"slope_lowCI"=NA,"slope_highCI"=NA))
  }
  
  ts_usage <- xts(usage, order.by = timestamp)
  #verify trend (using zyp.trend.vector)
  zyp_output <- zyp.trend.vector(ts_usage, method=c('yuepilon'), conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
  if (zyp_output[6]  < 0.05) {
    trend = "Yes"
  } else {
    trend = "No"
  }
  #Estimate slope, CI
  time_diff <- mean(diff(timestamp))
  units(time_diff) <- "mins"
  factor_day <- 24*60/as.numeric(time_diff)
  slope_per_day <- zyp_output[2]*factor_day
  slope_per_day_lowCI <- zyp_output[1]*factor_day
  slope_per_day_highCI <- zyp_output[4]*factor_day
  intercept <- zyp_output[11]
  #package and return [ trend(Y/N), intercept, median_slope, 95%low, 95%high ]
  return(list("trend"=trend,"intercept"=intercept,"slope"=slope_per_day,"slope_lowCI"=slope_per_day_lowCI,"slope_highCI"=slope_per_day_highCI))
}