#' To conduct a parametric (normality assumed) test on the equality of means for two independent numerical groups.
#' @export 
myttest = function(x, y, alpha = 0.05, display = FALSE) {
  # Variances and lengths
  var_x = var(x)
  var_y = var(y)
  n_x = length(x)
  n_y = length(y)
  
  # Check variance
  F_stat = max(var_x, var_y) / min(var_x, var_y)
  F_crit = qf(1 - alpha / 2, df1 = n_x - 1, df2 = n_y - 1)
  equal_var = F_stat < F_crit
  
  # Means
  mean_x = mean(x)
  mean_y = mean(y)
  meandiff = mean_x - mean_y
  
  # Standard error & df
  if (equal_var) {
    sp2 = ((n_x - 1) * var_x + (n_y - 1) * var_y) / (n_x + n_y - 2)
    se = sqrt(sp2 * (1/n_x + 1/n_y))
    df = n_x + n_y - 2
  } else {
    se = sqrt(var_x / n_x + var_y / n_y)
    df = ( (var_x / n_x + var_y / n_y)^2 ) /
      ( ((var_x / n_x)^2 / (n_x - 1)) + ((var_y / n_y)^2 / (n_y - 1)) )
  }
  
  # t-stat and p-val
  t_stat = abs(meandiff / se)
  p_value = 2 * (1 - pt(t_stat, df))
  
  # CI
  t_crit = qt(1 - alpha/2, df)
  margin = t_crit * se
  CI = c(meandiff - margin, meandiff + margin)
  
  # histogram
  if (display) {
    par(mfrow = c(1, 2))
    hist(x, col = "skyblue", main = "Group X", xlab = "Values")
    hist(y, col = "salmon", main = "Group Y", xlab = "Values")
  }
  
  return(list(
    tstat = t_stat,
    pval = p_value,
    ci = CI,
    meandiff = meandiff,
    equal_variance_assumed = equal_var
  ))
}