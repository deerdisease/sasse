source(file.path('utils', 'beta_hpd.R'))

#' Derive parameters for a Beta distribution that has specific properties
#'
#' Use the Beta distribution's mode-concentration to try to find parameters that
#' yield concave distributions.  The parameters will try to ensure a) the 
#' interval I=(\code{lower}, \code{upper}) contains \code{size} x 100% of the 
#' distribution's probability mass, and b) the distribution's mode will be the 
#' midpoint of the interval I.
#'  
#' @param lower left endpoint of distribution's bulk
#' @param upper right endpoint of distribution's bulk
#' @param size probability mass contained in distribution's bulk
#'  
beta_params = function(lower=0, upper=.4, size = .95) {
  #browser()
  tgt_mode = (lower + upper) / 2
  tgt_interval = c(lower, upper)
  o = optim(
    par = c(0,0), 
    fn = function(x) {
      mode = plogis(q = x[1])
      concentration = exp(x[2])
      alpha = 1 + mode * concentration
      beta = 1 + concentration * (1-mode)
      diff = beta_hpd(alpha = alpha, beta = beta, level = size) - tgt_interval
      # match on interval and mode
      sum(diff^2) + sum((mode-tgt_mode)^2)
    }
  )
  mode = plogis(q = o$par[1])
  concentration = exp(o$par[2])
  alpha = 1 + mode * concentration
  beta = 1 + concentration * (1-mode)
  c(alpha, beta)
}
