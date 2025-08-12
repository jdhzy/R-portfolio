#' partition dataset
#' @export
tetr = function(Xk, Y = NULL, p = 0.65, seed = NULL) {
  if (p <= 0 || p >= 1) stop("p must be between 0 and 1")
  if (!is.data.frame(Xk)) Xk = as.data.frame(Xk)
  if (nrow(Xk) < 2) stop("Xk must have at least 2 rows")
  if (!is.null(Y) && length(Y) != nrow(Xk)) stop("Y must be the same length as the number of rows in Xk")
  
  if (!is.null(seed)) set.seed(seed)
  
  n = nrow(Xk)
  n_train = floor(n * p)
  
  index_train = sample(1:n, n_train, replace = FALSE)
  index_test = setdiff(1:n, index_train)
  
  Xtraining = Xk[index_train, , drop = FALSE]
  Xtesting = Xk[index_test, , drop = FALSE]
  
  if (!is.null(Y)) {
    Ytraining = Y[index_train]
    Ytesting = Y[index_test]
    return(list(Xtraining = Xtraining, Xtesting = Xtesting,
                Ytraining = Ytraining, Ytesting = Ytesting))
  } 
  else {
    return(list(Xtraining = Xtraining, Xtesting = Xtesting))
  }
}