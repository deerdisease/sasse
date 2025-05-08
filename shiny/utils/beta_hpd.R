# 95% highest density interval for a beta distribution
# 
# Parameters:
#  alpha - shape 1 parameter
#  beta - shape 2 parameter
#  level - confidence level
beta_hpd = function(alpha, beta, level = 0.95) {
  
  # process vector inputs
  res = t(mapply(FUN = function(alpha, beta) {
    
    # interval's lower bound must leave adequate room to aggregate prob. mass
    lower_max = qbeta(
      p = level, shape1 = alpha, shape2 = beta, lower.tail = FALSE
    )
    
    # find the lower bound for the highest density interval
    lower = optim(par = lower_max / 2, fn = function(lower) {
      # CDF value at LHS of interval, "F(lower)"
      plower = pbeta(q = lower, shape1 = alpha, shape2 = beta)
      # quantile fn uniquely identifies interval RHS "F^{-1}(F(lower) + level)"
      upper = qbeta(p = plower + level, shape1 = alpha, shape2 = beta)
      # interval width
      upper - lower
    }, method = 'Brent', lower = 0, upper = lower_max)$par
    
    # same code... find the interval RHS for optimal LHS
    plower = pbeta(q = lower, shape1 = alpha, shape2 = beta)
    upper = qbeta(p = plower + level, shape1 = alpha, shape2 = beta)
    
    # return interval
    c(lower, upper)
    
  }, alpha, beta, SIMPLIFY = TRUE))
  
  # package results
  colnames(res) = c('lower', 'upper')
  res
}
