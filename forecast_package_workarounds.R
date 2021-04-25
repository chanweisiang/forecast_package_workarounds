## Manual decomposition of time-series data
## Replacement for stl() and decompose() when seasinality could not be found
## Uses moving average to create the trend component

man_decomp_plot <- function(data_series, ma_order){
  ## Trend
  trend <- ma(data_series, order = ma_order, centre = TRUE)
  
  ## Seasonality
  detrended <- data_series - trend
  seasonality_order <- findfrequency(detrended)
  seasonality_matrix <- t(matrix(data = detrended, nrow = seasonality_order))
  seasonal_avg = colMeans(seasonality_matrix, na.rm = TRUE)
  
  ## Noise
  noise_w_seasonality <- data_series - trend - seasonal_avg
  noise_w_no_seasonality <- data_series - trend
  
  ## Decomposition plot
  if (seasonality_order > 1) {
    plot(cbind(data_series, trend, seasonal_avg, noise_w_seasonality),
         main = "", yax.flip = TRUE, cex.lab = 0.75)
  } else {
    plot(cbind(data_series, trend, noise_w_no_seasonality),
         main = "", yax.flip = TRUE, cex.lab = 0.75)
  }
}


## The following are functions for calculating MAPE when there are zeroes in the series
## It performs the calculations by ignoring the zeroes and only calculating for thos with values

### MAPE function for training data series
### "forecast" is the output from your ets(), Arima(), and auto.arima() functions.
### "actual" is your original series.

mape_train_func <- function(forecast, actual){
  div_series <- abs(forecast[["fitted"]] - actual) * 100 / actual
  na_series <- sapply(div_series, function(x) replace(x, is.infinite(x), NA))
  sum(na_series, na.rm = TRUE) / (length(na_series) - sum(is.na(na_series)))
}


### MAPE function for forecast series
### "forecast" is the output from your forecast() function.
### "actual" is your original series.

mape_fore_func <- function(forecast, actual){
  sum(abs((forecast[["mean"]] - actual) / actual), na.rm = TRUE) /
    length(which(!is.na(actual)))
}


### MAPE functions for tsCV() analysis
### "h" and "forecast_period" are your forecast period.
### "data_series" refers to your original series
### "actual" is the output of matrix_lagged_values()
### "error" is the output of tsCV

matrix_lagged_values <- function(data_series, forecast_period){
  rollsum <- as.data.frame(shift(data_series, 
                                 0:forecast_period, type = "lead"))
  names <- c(sprintf("L%03d", seq(0, forecast_period)))
  colnames(rollsum) <- names
  return(rollsum)
}

mape_matrix_func <- function(error, actual, h){
  error_matrix <- as.data.frame(abs(error))
  actual_matrix <- actual[, 1:h + 1]
  div_matrix <- as.data.frame(error_matrix / actual_matrix)
  na_matrix <- sapply(div_matrix, function(x) replace(x, is.infinite(x), NA))
  APE <- sum(na_matrix, na.rm = TRUE)
  APE / (length(na_matrix) - sum(is.na(na_matrix)))
}
