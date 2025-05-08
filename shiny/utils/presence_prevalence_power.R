source(file.path('utils', 'presence_prevalence_estimation.R'))
library(dplyr)

#' Power for presence-prevalence models
#' 
#' Model assumes Bernoulli sampling of an infinite population.  Exact power 
#' calculations are relatively fast to evaluate when sample size \code{n} is 
#' small.  Monte Carlo approximation will be used for large sample sizes.  The
#' switch from exact to approximate power calculations occurs when 
#' \code{n > mciter}, i.e., when the support for possible study outcomes exceeds
#' the computational cost of a Monte Carlo approximation.
#' 
#' @param n sample size
#' @param prior_prevalence a vector \code{c(shape1, shape2)} parameterizing a 
#'   beta prior distribution for prevalence
#' @param prior_disease prior probability that there is disease in a population
#' @param sensitivity probability a positive animal yields a positive test
#' @param specificity probability a negative animal yields a negative test
#' @param true_prevalence true prevalence for disease in population
#' @param freedom_signif significance level at which to test whether population
#'   is free from disease
#' @param disease_signif significance level at which to test whether population
#'   has disease
#' @param prevalence_conf target probability content of highest posterior 
#'   density interval for prevalence
#' @param prevalence_quantile quantile of the posterior density for prevalence 
#'   to return as a posterior upper bound for prevalence
#' @param mciter number of Monte Carlo simulations to use to approximate power 
#'   when n > mciter.
#'
#' @return \code{data.frame} of summary statistics about statistical power
#'   for presence and prevalence
#'   
#' @examples 
#' presence_prevalence_power(
#'   n = 100, prior_prevalence = c(shape1 = 1, shape2 = 10), prior_disease = .2, 
#'   sensitivity = .95, specificity = .95, true_prevalence = 0, mciter = 1e4
#' )
#'
presence_prevalence_power = function(
    n, prior_prevalence, prior_disease, sensitivity, specificity, 
    true_prevalence, freedom_signif = .05, disease_signif = .05, 
    prevalence_conf = .95, prevalence_quantile = .95, mciter = 1e4
) {
  
  n = as.numeric(n)
  prior_prevalence = as.numeric(prior_prevalence)
  sensitivity = as.numeric(sensitivity)
  specificity = as.numeric(specificity)
  true_prevalence = as.numeric(true_prevalence)
  freedom_signif = as.numeric(freedom_signif)
  prevalence_conf = as.numeric(prevalence_conf)
  prevalence_quantile = as.numeric(prevalence_quantile)
  mciter = as.numeric(mciter)
  
  stopifnot(
    'Sample size n must be a scalar' = length(n) == 1,
    'Sample size n must be greater than 0' = all(n > 0),
    'Sample sizes n must be integers' = all(round(n) == n),
    'prior_prevalence parameters must be positive' = all(prior_prevalence > 0),
    'prior_disease must be a scalar' = length(prior_disease) == 1,
    'prior_disease must be non-negative' = all(prior_disease >= 0),
    'prior_disease must be no larger than 1' = all(prior_disease <= 1),
    'sensitivity must be a scalar' = length(sensitivity) == 1,
    'sensitivity must be non-negative' = all(sensitivity >= 0),
    'sensitivity must be no larger than 1' = all(sensitivity <= 1),
    'specificity must be a scalar' = length(specificity) == 1,
    'specificity must be non-negative' = all(specificity >= 0),
    'specificity must be no larger than 1' = all(specificity <= 1),
    'true_prevalence must be a scalar' = length(true_prevalence) == 1,
    'true_prevalence must be non-negative' = all(true_prevalence >= 0),
    'true_prevalence must be no larger than 1' = all(true_prevalence <= 1),
    'Monte Carlo sample size mciter must be a scalar' = length(mciter) == 1,
    'Monte Carlo sample size mciter must be greater than 0' = all(mciter > 0)
  )
  
  # test positivity rate, for simulation
  pos_rate = true_prevalence * sensitivity + 
    (1 - true_prevalence) * (1 - specificity)
  
  # sample outcomes to evaluate
  if(mciter > n) {
    # explore entire distribution
    study_data = data.frame(
      npos = 0:n, 
      prob = dbinom(x = 0:n, size = n, prob = pos_rate)
    )
  } else {
    # monte carlo subset to investigate
    study_data = data.frame(table(
      rbinom(n = mciter, size = n, prob = pos_rate)
    ))
    colnames(study_data) = c('npos', 'prob')
    study_data$npos = as.numeric(format(study_data$npos))
    study_data$prob = study_data$prob / mciter
  }
  
  # evaluate posterior distributions
  study_outcomes = presence_prevalence_estimation(
    x = study_data$npos, 
    n = n, 
    prior_prevalence = prior_prevalence, 
    prevalence_conf = prevalence_conf,
    prevalence_quantile = prevalence_quantile,
    prior_disease = prior_disease, 
    sensitivity = sensitivity, 
    specificity = specificity
  )
  
  # summarize study_outcome results, accounting for weighting
  study_outcomes %>% 
    left_join(
      y = study_data,
      by = c('x' = 'npos')
    ) %>% 
    mutate(
      signif_disease_freedom = prob_disease < freedom_signif,
      signif_disease = prob_disease_freedom < disease_signif,
      covered = (prevalence_hpd_lower <= true_prevalence) & 
        (true_prevalence <= prevalence_hpd_upper),
      bounded = true_prevalence <= prevalence_bound,
      ci_width = prevalence_hpd_upper - prevalence_hpd_lower,
      prevalence_cv = prevalence_sd / prevalence_mean
    ) %>% 
    group_by(1) %>%
    summarise(
      prob_signif_disease_freedom = sum(signif_disease_freedom * prob),
      prob_signif_disease = sum(signif_disease * prob),
      prob_prevalence_hpd_covers = sum(covered * prob),
      prob_prevalence_bounded = sum(bounded * prob),
      mean_prevalence_ci_width = sum(ci_width * prob),
      mean_prevalence_bound = sum(prevalence_bound * prob),
      mean_prevalence_cv = sum(prevalence_cv * prob)
    ) %>% 
    ungroup() %>% 
    select(-1)
}
