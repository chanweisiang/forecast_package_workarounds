# forecast_package_workarounds
A problem I encountered with the forecast package is the "inf" MAPE calculated when there are zeroes in the data.
Rob Hyndman has highlighted the weakness of Mean Absolute Percentage Error (MAPE) when numbers are very small.
When there are zeroes in your series, the denominator becomes 0 causing your mape to become infinite.
To workaround this problem, I created some functions to calculate MAPEs by bypassing the zeroes in the series.
I also created a function for calculating MAPEs from the output of the tsCV() function.
