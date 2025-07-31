library(nimble)
library(coda)

invisible(
  lapply(dir(path = 'utils', pattern = '*.R', full.names = TRUE), source)
)

#
# example input 
#

prior_presence = .2

sampling_population_priors = rbind(
  data.frame(name = 'group 1', prevalence_lower = 0, prevalence_upper = .6)
)

data = rbind(
  data.frame(
    population = 'group 1', 
    sample_size = 60, 
    positive_tests = 2, 
    sensitivity = .95, 
    specificity = .95
  ),
  data.frame(
    population = 'group 1',
    sample_size = 10,
    positive_tests = 0,
    sensitivity = .5,
    specificity = .95
  )
)

#
# modeling
#

prior_prevalence_parameters = t(apply(
  X = sampling_population_priors,
  MARGIN = 1, 
  FUN = function(r) {
    beta_params(
      lower = as.numeric(r['prevalence_lower']),
      upper = as.numeric(r['prevalence_upper'])
    )
  }
))

template_model = nimbleCode({
  
  presence ~ dbinom(size = 1, prob = prior_presence)
  
  for(i in 1:n_populations) {
    
    prevalence_latent[i] ~ dbeta(
      shape1 = prior_prevalence[i, 1], shape2 = prior_prevalence[i, 2]
    )
    
    prevalence[i] <- prevalence_latent[i] * presence
  }
  
  for(i in 1:n_samples) {
    npos[i] ~ dbinom(
      size = sample_size[i],
      prob = sensitivity[i] * prevalence[npos_prevalence[i]] + 
        (1 - specificity[i]) * (1 - prevalence[npos_prevalence[i]])
    )
  }
  
})

pkg = list(
  data = list(
    npos = data$positive_tests
  ),
  inits = list(
    presence = 1,
    prevalence_latent = prior_prevalence_parameters[,1] / 
      rowSums(prior_prevalence_parameters),
    prevalence = prior_prevalence_parameters[,1] / 
      rowSums(prior_prevalence_parameters)
  ),
  constants = list(
    prior_presence = prior_presence,
    n_populations = nrow(sampling_population_priors),
    prior_prevalence = prior_prevalence_parameters,
    n_samples = nrow(data),
    sample_size = c(data$sample_size, NA), # ensure dimensionality is preserved
    sensitivity = c(data$sensitivity, NA), # ensure dimensionality is preserved
    specificity = c(data$specificity, NA), # ensure dimensionality is preserved
    npos_prevalence = c(
      match(
        x = data$population, 
        table = sampling_population_priors$name
      ),
      NA # ensure dimensionality is preserved
    )
  )
)

mod = nimbleModel(
  code = template_model, constants = pkg$constants, data = pkg$data, 
  inits = pkg$inits
)

cmod = compileNimble(mod)

if(!is.finite(cmod$calculate())) {
  stop('Non-finite likelihood')
}

cfg = configureMCMC(model = mod)

cfg$resetMonitors()
cfg$addMonitors(c('presence', 'prevalence'))

sampler = buildMCMC(cfg)

csampler = compileNimble(sampler)

csampler$run(niter = 1e6)

samples = as.matrix(csampler$mvSamples)

post_inds = unique(round(seq(
  from = nrow(samples) / 2, 
  to = nrow(samples), 
  length.out = 1e4
)))

m = mcmc(samples[post_inds,])

#
# posterior distribution output
#

# posterior quantile to use for prevalence bound
prevalence_quantile = .95

# model nodes containing prevalence samples
prevalence_tgts = paste0('prevalence[', 1:pkg$constants$n_populations, ']')

# posterior summaries
prob_disease = mean(m[,'presence'] == 1)
prob_disease_freedom = mean(m[,'presence'] == 0)
prevalence_mean = colMeans(m[, prevalence_tgts, drop = FALSE])
prevalence_sd = apply(
  X = m[, prevalence_tgts, drop = FALSE],
  MARGIN = 2,
  FUN = sd
)
prevalence_hpd = HPDinterval(m[, prevalence_tgts, drop = FALSE])
prevalence_hpd_lower = prevalence_hpd[,'lower']
prevalence_hpd_upper = prevalence_hpd[,'upper']
prevalence_bound = apply(
  X = m[, prevalence_tgts, drop = FALSE],
  MARGIN = 2,
  FUN = quantile, 
  p = prevalence_quantile
)

