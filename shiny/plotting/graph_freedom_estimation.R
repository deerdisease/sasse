#' Freedom estimation visualization
#' 
#' Graphs to visualize prior vs posterior comparison for freedom analysis
#' 
#' @param lower_hpd lower bound of prior 
#' @param upper_hpd upper bound of prior
#'
#' @return graph for shiny
#'   
 
graph_freedom_estimation = function(lower_hpd, upper_hpd, prior, p, s, sens, spec){

# load utilities
invisible(
  lapply(dir(path = 'utils', pattern = '*.R', full.names = TRUE), source)
)

# TODO: make user input
# specify prior distribution via bounds
prior_hpd = c(lower = lower_hpd, upper = upper_hpd)
# specify prior probability that disease is present
prior_disease = prior

# TODO: make user input
observations = c(npos = p, nsamples = s)

# analyze prior distribution
prior_params = beta_params(
  lower = prior_hpd['lower'], 
  upper = prior_hpd['upper']
)

# evaluate posterior distribution
post = presence_prevalence_estimation(
  x = observations['npos'],
  n = observations['nsamples'],
  prior_prevalence = prior_params,
  prior_disease = prior_disease,
  sensitivity = sens, # TODO: make user input
  specificity = spec, # TODO: make user input
  prevalence_conf = .95, 
  prevalence_quantile = .95
)

# extract posterior components
post_freedom = post$prob_disease_freedom
post_confirmation = post$prob_disease
post_prevalence_bound = post$prevalence_bound

# prior/posterior linkage points
linkage_controls = data.frame(
  x = c(0,1,4,5),
  y = c(rep(prior_disease, 2), rep(post_confirmation, 2))
)
linkage_df = data.frame(
  predict(interpSpline(linkage_controls$x, linkage_controls$y))
) %>% 
  # restrict ribbon to plotting range
  filter(
    x >= 1, x <= 4
  ) %>% 
  # fix potential spline interpolation issues
  mutate(
    y = ifelse(y < 0, 0, y),
    y = ifelse(y > 1, 1, y)
  )

# power bi ribbon colors: https://learn.microsoft.com/en-us/power-bi/visuals/desktop-ribbon-charts?tabs=powerbi-desktop
ribbon_col = rgb(112, 188, 208, maxColorValue = 255)
ribbon_point = '#2c7f94'
data_point = rgb(233, 198, 69, maxColorValue = 255)

# prior vs posterior disease presence/confirmation
ggplot() +
  # prior/posterior comparison
  geom_line(
    data = linkage_df,
    mapping = aes(x = x, y = y),
    col = ribbon_col
  ) + 
  geom_point(
    data = data.frame(
      x = c(1, 4),
      y = c(prior_disease, post_confirmation)
    ),
    mapping = aes(x = x, y = y),
    shape = 23,
    size = 4,
    col = 'white', 
    fill = ribbon_point
  ) + 
  # data overlay
  geom_point(
    data = data.frame(
      x = 2.5,
      y = ifelse(observations['npos'] > 0, 1, 0)
    ),
    mapping = aes(x = x, y = y),
    shape = 23,
    size = 4,
    col = 'white', 
    fill = data_point
  ) + 
  # formatting
  scale_x_continuous(
    breaks = c(1, 2.5, 4),
    labels = c('Prior', 'Data', 'Posterior')
  ) + 
  scale_y_continuous(
    'Disease presence',
    breaks = seq(from = 0, to = 1, by = .2),
    labels = scales::percent,
    limits = c(0, 1)
  ) + 
  theme_few() + 
  theme(
    panel.grid.major.y = element_line(colour = 'grey75'),
    panel.grid.minor.y = element_line(colour = 'grey80'),
    axis.title.x = element_blank()
  ) +
  ggtitle("What is the probability the population is free from disease?")

}
