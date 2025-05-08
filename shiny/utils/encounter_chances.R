#' Probability at least one positive animal is captured in sample
#' 
#' Assumes disease is present in population
#' 
#' @param n vector of sample sizes
#' @param p vector of prevalences
#'
#' @return vector of probabilities that there will be at least one positive 
#'   animal in a sample of size \code{n} with prevalence \code{p}
#'   
#' @examples 
#' encounter_chances(n = 1:10, p = .2)
#'   
encounter_chances = function(n, p) {
  stopifnot(
    'Sample sizes n must be greater than 0' = all(n > 0),
    'Prevalences p must be non-negative' = all(p >= 0),
    'Prevalences p must be no larger than 1' = all(p <= 1)
  )
  1 - (1 - p)^n
}
