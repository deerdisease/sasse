library(splines2)
library(lsei)

#' Routine 
#'
#'
#'
#'
power_study_interpolation = function(x, y, x0, tol = .01) {
  
  # support the spline on the full range of data and predictions needed
  Boundary.knots = range(c(x,x0))
  
  # we know all data to which this function will be applied represent some sort 
  # of probability and are at most machine precision larger than 1, so truncate 
  # as needed to simplify follow-on plotting requirements
  if(any(y > 1)) {
    y[y > 1] = 1
  }
  
  # ispline degrees of freedom set for interpolation
  ispline_degree = length(x) - 1
  
  # regression design matrix, manually adding an intercept
  X = cbind(
    1, iSpline(x = x, degree = ispline_degree, Boundary.knots = Boundary.knots)
  )
  
  # predictor design matrix
  Xn = cbind(
    1, iSpline(x = x0, degree = ispline_degree, 
               Boundary.knots = Boundary.knots)
  )
  
  # first, try a monotonically decreasing spline fit
  fit = nnls(X, 1 - y)$x
  if(all(abs((1 - X %*% fit) - y) <= tol)) {
    # return interpolation
    return(as.numeric(1 - Xn %*% fit))
  }
  
  # next, try a monotonically increasing spline fit
  fit = nnls(X, y)$x
  if(all(abs((1 - X %*% fit) - y) <= tol)) {
    # return interpolation
    return(as.numeric(Xn %*% fit))
  }
  
  # attempt to fail-back with a standard LOESS fit
  fit = loess(y~x, data.frame(x = x, y = y))
  if(!all(abs(fit$residuals) <= tol)) {
    warning('Interpolation tolerances not satisfied, results may be inaccurate')
  }
  suppressWarnings({
    y0 = as.numeric(predict(fit, data.frame(x = x0)))
  })
  if(any(y0<0)) { y0[y0<0] = 0 }
  if(any(y0>1)) { y0[y0>1] = 1 }
  return(y0)
  
}
