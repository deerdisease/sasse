library(bisque)

#' Posterior probability for prevalence
#' 
#' Model assumes Bernoulli sampling of an infinite population
#' 
#' @param x vector with number of positive test results for each sample
#' @param n sample size
#' @param prior_prevalence a vector \code{c(shape1, shape2)} parameterizing a 
#'   beta prior distribution for prevalence
#' @param prior_disease prior probability that there is disease in a population
#' @param sensitivity probability a positive animal yields a positive test
#' @param specificity probability a negative animal yields a negative test
#' @param prevalence_conf target probability content of highest posterior 
#'   density interval for prevalence
#' @param prevalence_quantile quantile of the posterior density for prevalence 
#'   to return as a posterior upper bound for prevalence
#' @param pmf \code{TRUE} to return the pmf evaluated at \code{n.pmf} points 
#'   within the \code{prevalence_conf} interval
#'
#' @return \code{data.frame} of summary statistics about posterior distribution 
#'   for presence and prevalence
#'   
#' @examples 
#' presence_prevalence_estimation(
#'   x = 2, 
#'   n = 10, 
#'   prior_prevalence = c(shape1 = 1, shape2 = 10), 
#'   prior_disease = .2, 
#'   sensitivity = .9, 
#'   specificity = .9
#' )
#' 
presence_prevalence_estimation = function(
    x=10, n=100, prior_prevalence = c(3,12), prior_disease = .5, sensitivity = .95, specificity = .95,
    prevalence_conf = .95, prevalence_quantile = .95, pmf = FALSE, n.pmf = 100
) {
  
  stopifnot(
    'Sample size n must be a scalar' = length(n) == 1,
    'Sample size n must be greater than 0' = all(n > 0),
    'Number of positive tests x must be non-negative' = all(x >= 0),
    'prior_prevalence parameters must be positive' = all(prior_prevalence > 0),
    'prior_disease must be a scalar' = length(prior_disease) == 1,
    'prior_disease must be non-negative' = all(prior_disease >= 0),
    'prior_disease must be no larger than 1' = all(prior_disease <= 1),
    'sensitivity must be a scalar' = length(sensitivity) == 1,
    'sensitivity must be non-negative' = all(sensitivity >= 0),
    'sensitivity must be no larger than 1' = all(sensitivity <= 1),
    'specificity must be a scalar' = length(specificity) == 1,
    'specificity must be non-negative' = all(specificity >= 0),
    'specificity must be no larger than 1' = all(specificity <= 1)
  )
  
  post_prevalence_proportional = function(p, x) {
    # Function proportional to posterior for prevalence, including atom at p=0
    # 
    # Parameters:
    #   p - vector of prevalence values
    #   x - scalar for number of positive tests
    stopifnot('x should be scalar' = length(x) == 1)
    # component representing sampling in the presence of disease
    (p!=0) *
      # likelihood 
      dbinom(
        x = x, 
        size = n, 
        prob = sensitivity * p + (1 - specificity) * (1 - p)
      ) * 
      # prior
      dbeta(
        x = p, 
        shape1 = prior_prevalence[1],
        shape2 = prior_prevalence[2]
      ) *
      prior_disease +
    # atom representing sampling under disease freedom
    (p==0) * 
      # likelihood
      dbinom(
        x = x, 
        size = n, 
        prob = 1 - specificity
      ) *
      # prior
      (1 - prior_disease) 
  }
  
  # vectorize over varying sample sizes
  res = lapply(x, function(x) {
    
    #
    # find posterior mode via transformed parameter space
    #
    
    post_mode_fn = function(theta) {
      # log-posterior
      log(post_prevalence_proportional(p = plogis(q = theta), x = x)) + 
      # log-jacobian of transformation
      jac.logit(theta)
    }
    
    # test empirical mean as initial value
    post_mode_init = qlogis(x/n)
    invalid_init = !is.finite(post_mode_fn(post_mode_init))
    # test large value as initial value if alternates are not suitable
    if(invalid_init) {
      post_mode_init = qlogis(1 - .Machine$double.eps)
      invalid_init = !is.finite(post_mode_fn(post_mode_init))
    }
    # test small value as initial value if alternates are not suitable
    if(invalid_init) {
      post_mode_init = qlogis(.Machine$double.eps)
      invalid_init = !is.finite(post_mode_fn(post_mode_init))
    }
    # test prior mean as initial value if alternates are not suitable
    if(invalid_init) {
      post_mode_init = qlogis(prior_prevalence[1] / sum(prior_prevalence))
      invalid_init = !is.finite(post_mode_fn(post_mode_init))
    }
    # try basic grid search to find init
    for(post_mode_init in qlogis(seq(from = .01, to = .99, by = .01))) {
      invalid_init = !is.finite(post_mode_fn(post_mode_init))
      if(!invalid_init) {
        break
      }
    }
    
    # find posterior mode
    post_mode = suppressWarnings(
      plogis(
        q = optim(
          par = post_mode_init,
          fn = post_mode_fn,
          method = 'Nelder-Mead',
          control = list(fnscale = -1)
        )$par
      )
    )
    
    # unnormalized mass of non-zero posterior distribution component
    nonzero_conditional_normalization_constant = integrate(
      f = post_prevalence_proportional, lower = 0, upper = 1, x = x
    )$value
    
    # overall posterior normalization constant
    normalization_constant = nonzero_conditional_normalization_constant + 
      post_prevalence_proportional(p = 0, x = x)
    
    # overall posterior distribution
    post_pmf = function(p, nonzero = FALSE) {
      # Parameters:
      #  p - value at which to evaluate posterior density
      #  nonzero - specifies whether to evaluate posterior density conditional
      #   on the event that p > 0 (note: conditioning in a probability sense
      #   vs. a Bayesian sense)
      if(isFALSE(nonzero)) {
        post_prevalence_proportional(p = p, x = x) / normalization_constant
      } else {
        post_prevalence_proportional(p = p + .Machine$double.eps, x = x) / 
          nonzero_conditional_normalization_constant
      }
    }
    
    mass_at_zero = post_pmf(p = 0)
    
    post_cdf_raw = function(p, nonzero = FALSE) {
      if(isFALSE(nonzero)) {
        integrate(
          f = post_pmf, 
          lower = 0, 
          upper = p
        )$value + 
          mass_at_zero
      } else {
        integrate(
          f = post_pmf,
          lower = .Machine$double.eps,
          upper = p
        )$value
      }
    }
    
    post_cdf_rescaled = c(
      'TRUE' = post_cdf_raw(p = 1, nonzero = TRUE),
      'FALSE' = post_cdf_raw(p = 1, nonzero = FALSE)
    )
    
    post_cdf = function(p, nonzero = FALSE) {
      post_cdf_raw(p = p, nonzero = nonzero) / 
        post_cdf_rescaled[as.character(nonzero)]
    }
    
    post_quantile = function(p, nonzero = FALSE) {
      suppressWarnings(
        plogis(
          optim(
            par = qlogis(post_mode), 
            fn = function(s) 
              abs(post_cdf(p = plogis(s), nonzero = nonzero) - p), 
            method = 'Nelder-Mead'
          )$par
        )
      )
    }

    # highest posterior density interval for a distribution with an atom at 0
    post_hpd = function(level) {
      # Note: Due to the atom at 0, there may be two solutions, one that 
      # includes the atom, and the other that doesn't.  
      
      # the interval that contains 0 may be one of two solutions
      interval_with_0 = c(lower = 0, upper = post_quantile(p = level))
      
      # HPD interval must contain atom at 0 since the non-zero portion of the 
      # distribution doesn't have enough mass, so hpd interval is trivial!
      if(1 - mass_at_zero < level) {
        return(interval_with_0)
      }
      
      #
      # otherwise, find and compare with interval that does not contain zero
      #
      
      # interval's lower bound must leave adequate room to aggregate prob. mass
      lower_max = post_quantile(p = 1 - level, nonzero = TRUE)
      # find the lower bound for the highest density interval
      lower = optim(par = lower_max / 2, fn = function(lower) {
        # CDF value at LHS of interval, "F(lower)"
        plower = post_cdf(p = lower, nonzero = TRUE)
        # quantile fn uniquely identifies interval RHS "F^{-1}(F(lower) + level)"
        upper = post_quantile(p = plower + level, nonzero = TRUE)
        # interval width
        upper - lower
      }, method = 'Brent', lower = 0, upper = lower_max)$par
      # same code... find the interval RHS for optimal LHS
      plower = post_cdf(p = lower, nonzero = TRUE)
      upper = post_quantile(p = plower + level, nonzero = TRUE)
      interval_without_0 = c(lower = lower, upper = upper)
      # return the narrower of the two intervals
      if(abs(diff(interval_with_0)) < abs(diff(interval_without_0))) {
        return(interval_with_0)
      } else {
        return(interval_without_0)
      } 
    }
    
    # posterior mean for prevalence; only need to integrate over non-zero values
    post_mean = integrate(
      f = function(p) p * post_pmf(p = p), 
      lower = 0, 
      upper = 1
    )$value
    
    # posterior variance for prevalence
    post_var = integrate(
      f = function(p) (p - post_mean)^2 * post_pmf(p = p),
      lower = 0,
      upper = 1
    )$value
    
    # evaluate posterior hpd interval for prevalence
    hpd = post_hpd(level = prevalence_conf)
    
    # package results
    res = data.frame(
      x = x, 
      prob_disease = 1 - mass_at_zero,
      prob_disease_freedom = mass_at_zero,
      prevalence_mean = post_mean,
      prevalence_sd = sqrt(post_var),
      prevalence_hpd_lower = hpd['lower'],
      prevalence_hpd_upper = hpd['upper'],
      prevalence_bound = post_quantile(p = prevalence_quantile)
    )
    
    rownames(res) = NULL
    
    if(isFALSE(pmf)) {
      res
    } else {
      pseq = seq(
        from = res$prevalence_hpd_lower,
        to = res$prevalence_hpd_upper, 
        length.out = n.pmf
      )
      list(
        summaries = res,
        pmf = data.frame(
          p = pseq,
          pmf = post_pmf(p = pseq)
        )
      )
    }
  })
  
  if(isFALSE(pmf)) {
    res = do.call(rbind, res)
  } else {
    if(length(res) == 1) {
      res = res[[1]]
    }
  }

  res
}
