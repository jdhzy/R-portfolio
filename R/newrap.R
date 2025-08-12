#' approximate the solution for equations of the form f(x) = 0 via the Newton-Raphson iteration method
#' @export
newrap = function(f, x0, tol = 1e-6, maxiter = 250) {
  h = 1e-7
  x = x0
  iterates = c()
  
  for (i in 1:maxiter) {
    fval = f(x)
    df = (f(x + h) - f(x - h)) / (2 * h)
    
    if (abs(df) < 1e-10) {
      warning("Derivative near zero; stopping to avoid division by zero")
      break
    }
    
    x_new = x - fval / df
    iterates = c(iterates, x_new)
    
    if (abs(x_new - x) < tol) {
      return(list(solution = x_new, iterates = iterates, converged = TRUE))
    }
    
    x = x_new
  }
  
  return(list(solution = x, iterates = iterates, converged = FALSE))
}