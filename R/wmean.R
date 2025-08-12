#' Weighted mean
#' @export
wmean = function(x, w) {
  if (length(x) != length(w)) stop("x and w must be the same length")
  if (sum(w) == 0) stop("Sum of weights cannot be zero")
  
  sum(x * w) / sum(w)
}