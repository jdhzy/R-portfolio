#' Calculate the impurity/entropy of a vector.
#' @export

impure = function(vec, type = 'Shannon') {
  p = prop.table(table(vec))  
  
  if (type == "Gini") {
    return(1 - sum(p^2))
  } else if (type == "Shannon") {
    return(-sum(p * log2(p)))  
  } else if (type == "Hartley") {
    return(-sum(p * log10(p))) 
  } else if (type == "Natural") {
    return(-sum(p * log(p))) 
  } else {
    stop("Invalid type. Use 'Shannon', 'Hartley', 'Natural', or 'Gini'.")
  }
}