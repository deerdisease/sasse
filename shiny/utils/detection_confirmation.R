#' Posterior probability a sample provides evidence for disease
#' 
#' Model assumes Bernoulli sampling of an infinite population
#' 
#' @param x vector with number of positive test results for each sample
#' @param n sample size
#' @param p prevalence if there is disease
#' @param prior_disease prior probability that there is disease in population
#' @param sensitivity probability a positive animal yields a positive test
#' @param specificity probability a negative animal yields a negative test
#'
#' @return vector of probabilities that each sample provides evidence that the 
#'   population has disease, given sampling results
#'   
#' @example 
#' detection_confirmation(
#'   x = 1, 
#'   n = 10, 
#'   p = .2, 
#'   prior_disease = .2, 
#'   sensitivity = .9, 
#'   specificity = .9
#' )
#'   
detection_confirmation = function(
    x, n, p, prior_disease, sensitivity, specificity
) {
  
  stopifnot(
    'Number of positive tests x must be non-negative' = all(x >= 0),
    'Sample size n must be a scalar' = length(n) == 1,
    'Sample size n must be greater than 0' = all(n > 0),
    'Prevalence p must be a scalar' = length(p) == 1,
    'Prevalence p must be non-negative' = all(p >= 0),
    'Prevalence p must be no larger than 1' = all(p <= 1),
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
  
  # TODO: replace with model that allows prevalence to be estimated too?
  
  lik_with_disease = prior_disease *
    dbinom(x = x, size = n, prob = sensitivity * p + (1 - specificity) * (1-p))
  lik_without_disease = (1 - prior_disease) * 
    dbinom(x = x , size = n, prob = 1 - specificity)
  
  lik_with_disease / (lik_with_disease + lik_without_disease)
}
