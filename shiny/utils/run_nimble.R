library(nimble)
library(coda)

run_nimble = function(sampling_population_priors, data, prior_presence){
  



# run nimble

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
    sample_size = c(data$sample_size, NA),
    sensitivity = c(data$sensitivity,NA),
    specificity = c(data$specificity,NA),
    npos_prevalence = c(match(
      x = data$population, 
      table = sampling_population_priors$name
    ), NA)
    # ensure dimensionality is preserved
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

samples


}