#' perform the primary calculations associate to
#' @export
myknn = function(query, data, class_labels, K) {
  # Euclidean distances
  distances = apply(data, 1, function(row) sqrt(sum((row - query)^2)))
  
  # Get indices of K closest neighbors
  KClosestIndices = order(distances)[1:K]
  
  # Get classes of those K neighbors
  KClosestClasses = class_labels[KClosestIndices]
  
  yhat = names(which.max(table(KClosestClasses)))
  
  return(list(
    distance = distances,
    KClosestIndices = KClosestIndices,
    KClosestClasses = KClosestClasses,
    yhat = yhat
  ))
}