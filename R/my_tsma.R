#' To construct a moving-average (MA[w]) approximation to a time series
#' @export
my_tsma = function(Y, w = 5) {
  n = length(Y)
  
  if (w > n) {
    stop("Window size w is larger than the length of the input vector Y.")
  }
  
  approx = rep(NA, n)
  
  for (i in w:n) {
    approx[i] = mean(Y[(i - w + 1):i], na.rm = TRUE)
  }
  
  return(list(
    original = Y,
    approx = approx,
    window = w
  ))
}