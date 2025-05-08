#' Prevalence estimation visualization
#' 
#' Graphs to visualize prior vs posterior comparison for freedom analysis
#' 
#' @param lower_hpd lower bound of prior 
#' @param upper_hpd upper bound of prior
#'
#' @return graph for shiny
#'   

graph_prevalence_estimation = function(lower_hpd, upper_hpd, data, post, prevalence_hpd_lower, prevalence_hpd_upper){


  # TODO: make user input
  # specify prior distribution via bounds
  prior_hpd = c(lower = lower_hpd, upper = upper_hpd)
  

  
  
  # analyze prior distribution
  prior_params = beta_params(
    lower = prior_hpd['lower'], 
    upper = prior_hpd['upper']
  )
  
  prior_mean = prior_params[1] / sum(prior_params)
  
  data <- data %>%
    unnest(cols = c(data))
  
  if(!is.null(data$positive_tests)){
    data <- data %>%
      mutate(x = 2.5, y = positive_tests/sample_size)
  }else if(is.null(data$positive_tests)){
    data <- data %>%
      as.data.frame(x = 2.5,y = 0)
  }

  
  # TODO: make user input
  #observations = c(npos = p, nsamples = s)
  
  

# extract posterior components
post_hpd = c(
  lower = prevalence_hpd_lower,
  upper = prevalence_hpd_upper
)
post_mean = post


## prior_hpd, post_hpd, prior_mean, post_mean, observations['npos'], observations['nsamples']




# hpd interval ribbon points
ribbon_controls = data.frame(
  x = c(0,1,4,5),
  ymax = c(rep(upper_hpd, 2), rep(post_hpd['upper'], 2)),
  ymin = c(rep(lower_hpd, 2), rep(post_hpd['lower'], 2))
)
ribbon_df = data.frame(
  upper = predict(interpSpline(ribbon_controls$x, ribbon_controls$ymax)),
  lower = predict(interpSpline(ribbon_controls$x, ribbon_controls$ymin))
) %>% 
  # format
  select(
    x = upper.x,
    lower = lower.y,
    upper = upper.y
  ) %>% 
  # restrict ribbon to plotting range
  filter(
    x >= 1, x <= 4
  ) %>% 
  # fix potential spline interpolation issues
  mutate(
    lower = ifelse(lower < 0, 0, lower),
    lower = ifelse(lower > 1, 1, lower),
    upper = ifelse(upper > 1, 1, upper),
    upper = ifelse(upper < 0, 0, upper)
  )

# closed, polygon version of ribbon
ribbon_poly = rbind(
  ribbon_df %>% select(x, y = upper),
  ribbon_df %>% select(x, y = lower) %>% map_df(rev)
)

# power bi ribbon colors: https://learn.microsoft.com/en-us/power-bi/visuals/desktop-ribbon-charts?tabs=powerbi-desktop
ribbon_col = rgb(112, 188, 208, maxColorValue = 255)
ribbon_point = '#2c7f94'
data_point = rgb(233, 198, 69, maxColorValue = 255)

# prior vs posterior plot, including data
ggplot() +
  # hpd comparison
  geom_polygon(
    data = ribbon_poly,
    mapping = aes(x = x, y = y),
    fill = ribbon_col,
    col = ribbon_col,
    alpha = .7
  ) + 
  # mean comparison
  geom_point(
    data = data.frame(
      x = c(1, 4),
      y = c(prior_mean, post_mean)
    ),
    mapping = aes(x = x, y = y),
    shape = 23,
    size = 4,
    col = 'white', 
    fill = ribbon_point
  ) + 
  # data overlay
  geom_point(
    mapping = aes(x = data$x, y = data$y),
    shape = 23,
    size = 4,
    col = 'white', 
    fill = data_point
  ) + 
  # formatting
  scale_x_continuous(
    breaks = c(1, 2.5, 4),
    labels = c('Initial expectation', 'Observation', 'Analytic result')
  ) + 
  scale_y_continuous(
    'Prevalence',
    breaks = seq(from = 0, to = 1, by = .2),
    labels = scales::percent,
    limits = c(0, 1)
  ) + 
  theme_few() + 
  theme(
    panel.grid.major.y = element_line(colour = 'grey75'),
    panel.grid.minor.y = element_line(colour = 'grey80'),
    axis.title.x = element_blank()
  )
}