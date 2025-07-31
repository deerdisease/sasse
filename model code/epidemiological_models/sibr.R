library(deSolve)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(dplyr)
library(ggpubr)
library(lubridate)
library(kableExtra)

#
# user input
#

# epi parameters
avg_infectious_days_input = 7
avg_recovery_days_input = 90
avg_waning_days_input = 365 * 1.5
R0 = 1.5

# TODO: add variation to reconfigure as standard SIR model (i.e., no B part)
# TODO: add variation to reconfigure as standard SIBR model (i.e., no waning)

# initial conditions at t=0
initial_susceptible = .999
initial_infectious = .001
initial_broadly_recovered = 0
initial_recovered = 0

# TODO: Make user input (i.e., so infection can start in spring vs winter, etc.)
# reference date for outbreak start
outbreak_start_date = ymd('2021-03-01')

# plotting window
plot_min_date = outbreak_start_date
plot_max_date = outbreak_start_date + duration(num = 3, units = 'years')

# diagnostic parameters
pathogenic_sensitivity = .95
pathogenic_specificity = .99
antibody_sensitivity = .95
antibody_specificity = .99

# annual demographics (could be list of input)
#   Note: set to NULL to ignore
birth_pulse_dates = data.frame(
  time = ymd('2021-06-01'),
  # proportion of total population that are new, susceptible animals after pulse
  proportion = c(.4) 
)
# birth_pulses = NULL

#
# solve model
#

# translate to average days in class
avg_infectious_days = avg_infectious_days_input
avg_recovery_days = avg_recovery_days_input - avg_infectious_days
avg_waning_days = avg_waning_days_input - avg_recovery_days

# convert birth pulse dates to model times (negatives are ok)
birth_pulses = birth_pulse_dates %>% 
  mutate(
    time = as.numeric(
      difftime(time1 = time, time2 = outbreak_start_date, units = 'days')
    )
  )

# clean up workspace
rm(sibr_solution)

# define model
sibr_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    ds = -beta * s * i + psi * r
    di =  beta * s * i - gamma * i
    db =  gamma * i - eta * b
    dr =  eta * b - psi * r
    return(list(c(ds, di, db, dr)))
  })
}

# parameterize model
gamma = 1 / avg_infectious_days
parameter_values = c(
  beta = R0 * gamma,
  gamma = gamma, 
  eta = 1 / avg_recovery_days,
  psi = 1 / avg_waning_days
)

# format initial conditions
initial_values = c(
  s = initial_susceptible,
  i = initial_infectious,
  b = initial_broadly_recovered,
  r = initial_recovered
)

# normalize initial conditions
initial_values = initial_values / sum(initial_values)

# set ode solver times, making sure we pass t=0 to solver
plot_min = as.numeric(
  difftime(time1 = plot_min_date, time2 = outbreak_start_date, units = 'days')
)
plot_max = as.numeric(
  difftime(time1 = plot_max_date, time2 = outbreak_start_date, units = 'days')
)

# set times to solve for inclusion in plot
plot_times = seq(from = plot_min, to = plot_max, length.out = 500)

# solve ode in chunks, following birth pulses
if(!is.null(birth_pulses)) {
  if(any(birth_pulses$time < plot_max)) {
    
    # enumerate time points with birth pulses
    homogeneous_period_ends = rbind(
      # birth pulse data
      do.call(rbind, apply(
        X = birth_pulses, 
        MARGIN = 1, 
        FUN = function(r) {
          data.frame(
            time = seq(from = r['time'], to = plot_max, by = 365),
            proportion = unname(r['proportion'])
          )
        }
      )),
      # final timepoint
      data.frame(time = plot_max, proportion = 0)
    ) %>% 
      # make sure times are after model start
      filter(time >= 0) %>% 
      # merge duplicates
      group_by(time) %>% 
      summarise(proportion = sum(proportion)) %>% 
      ungroup()
    
    # initialize ode solution
    sibr_solution = data.frame(time = 0, t(initial_values))
    
    # solve ode in chunks
    for(i in 1:nrow(homogeneous_period_ends)) {
      # time range during period
      t = unique(c(
        tail(sibr_solution$time, 1),
        data.frame(t = plot_times) %>% 
          filter(
            tail(sibr_solution$time, 1) <= t, 
            t <= homogeneous_period_ends$time[i]
          ) %>% 
          unlist() %>% 
          unname(),
        homogeneous_period_ends$time[i]
      ))
      # evaluate ode, but skip saving the first row since it duplicates start
      sibr_solution = rbind(
        sibr_solution,
        ode(
          y = initial_values,
          times = t,
          func = sibr_equations,
          parms = parameter_values
        )[-1,]
      )
      # apply birth pulse to last observation
      sibr_solution[nrow(sibr_solution), c('s','i','b','r')] = 
        sibr_solution[nrow(sibr_solution), c('s','i','b','r')] * 
          (1 - homogeneous_period_ends$proportion[i]) + 
        c(homogeneous_period_ends$proportion[i], rep(0, 3))
      # update initial conditions
      initial_values = c(
        s = tail(sibr_solution$s, 1),
        i = tail(sibr_solution$i, 1),
        b = tail(sibr_solution$b, 1),
        r = tail(sibr_solution$r, 1)
      )
      initial_values = initial_values / sum(initial_values)
    }
  }
}

# default: solve ode in one chunk
if(!exists('sibr_solution')) {
  t = unique(c(0, plot_times))
  sibr_solution = data.frame(
    ode(
      y = initial_values,
      times = t,
      func = sibr_equations,
      parms = parameter_values
    )
  ) 
}

# format plotting frame
sibr_solution = sibr_solution %>% 
  # restrict output to the requested plotting times
  filter(plot_min <= time, time <= plot_max) %>% 
  # enrich output
  mutate(
    # get date scale correct
    time = outbreak_start_date + duration(time, 'days'),
    # translate compartment model to ideal test results
    pathogenic_detection = (i + b) * pathogenic_sensitivity + 
      (s + r) * (1 - pathogenic_specificity),
    antibody_detection = (b + r) * antibody_sensitivity + 
      (s + i) * (1 - antibody_specificity)
  )

#
# output plots
#

# visualize epidemic trajectory
pl_trajectory = ggplot(
  data = sibr_solution %>% 
    pivot_longer(
      cols = c('s', 'i', 'b', 'r'), 
      names_to = 'compartment',
      values_to = 'population_proportion'
    ) %>% 
    mutate(
      compartment = factor(
        x = compartment,
        levels = c('s', 'i', 'b', 'r'),
        labels = c('Susceptible', 'Infectious', 'Broadly recovered', 
                   'Fully recovered')
      )
    ),
  mapping = aes(
    x = time, y = population_proportion, col = compartment)
) +
  geom_line() + 
  scale_x_datetime(date_labels = '%D') +
  scale_y_continuous(
    'Proportion of population',
    labels = scales::percent,
    limits = c(0,1)
  ) + 
  scale_color_brewer(type = 'qual', palette = 'Dark2') + 
  theme_few() + 
  theme(
    legend.title = element_blank(),
    panel.grid.major = element_line(colour = 'grey95'),
    axis.title.x = element_blank()
  )

# visualize marginal test rates
pl_test_positivity = ggplot(
  data = sibr_solution %>% 
    select(
      time, pathogenic_detection, antibody_detection
    ) %>% 
    pivot_longer(
      cols = c('pathogenic_detection', 'antibody_detection'),
      names_to = 'detection_type',
      values_to = 'positivity_rate'
    ) %>% 
    mutate(
      detection_type = factor(
        x = detection_type,
        levels = c('pathogenic_detection', 'antibody_detection'),
        labels = c('Positive pathogen tests', 'Positive antibody tests')
      )
    ),
  mapping = aes(x = time, y = positivity_rate, col = detection_type)
) + 
  geom_line() + 
  scale_color_brewer(
    type = 'qual', palette = 'Dark2'
  ) + 
  scale_x_datetime(date_labels = '%D') +
  scale_y_continuous(
    '(Ideal) Proportion of tests',
    labels = scales::percent,
    limits = c(0,1)
  ) + 
  theme_few() + 
  theme(
    legend.title = element_blank(),
    panel.grid.major = element_line(colour = 'grey95'),
    panel.grid.minor.x = element_line(colour = 'grey95'),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90)
  )

# TODO: dynamic x-axis plot scale
ggarrange(
  pl_trajectory, pl_test_positivity,
  ncol = 1,
  align = 'v'
)

# input data for almanac table
sibr_metrics = sibr_solution %>%
  mutate(
    r_overall = b + r
  ) %>% 
  # subset data
  select(
    Date = time,
    'Susceptible population' = s,
    'Infectious population' = i,
    'Recovered population' = r_overall,
    'Positive pathogen tests' = pathogenic_detection,
    'Positive antibody tests' = antibody_detection
  ) %>% 
  # re-format for summarization
  pivot_longer(
    cols = c('Susceptible population', 'Infectious population', 
             'Recovered population', 'Positive pathogen tests', 
             'Positive antibody tests'), 
    names_to = 'Metric', 
    values_to = 'Percentage'
  )

# output data for almanac table
sibr_almanac_long = rbind(
  # extract annual maxima
  sibr_metrics %>% 
    mutate(year = year(Date)) %>% 
    group_by(year, Metric) %>% 
    slice_max(Percentage) %>% 
    ungroup() %>% 
    # annotate output
    mutate(Value = 'Max.'),
  # extract annual minima
  sibr_metrics %>% 
    mutate(year = year(Date)) %>% 
    group_by(year, Metric) %>% 
    slice_min(Percentage) %>% 
    ungroup() %>% 
    # annotate output
    mutate(Value = 'Min.')
) %>% 
  # sort output
  arrange(
    Date
  ) %>% 
  # format output for display
  mutate(
    Percentage = paste0(
      format(
        x = round(Percentage * 100), 
        scientific = FALSE
      ),
      '%'
    )
  ) %>% 
  # reorder display
  select(Date, Metric, Value, Percentage)

#
# print condensed wide version of almanac table
#

superheader_names = c(
  '',
  'Population' = 4,
  'Positive tests' = 4
)

header_names = c(
  '',
  'Susceptible' = 2,
  'Infectious' = 2,
  'Pathogen' = 2,
  'Antibody' = 2
)

sibr_almanac_long %>%
  mutate(
    Year = year(Date),
    Cell_formatted = strftime(x = Date, format = '%m/%d')
  ) %>%
  pivot_wider(
    id_cols = Year,
    names_from = c('Metric', 'Value'),
    values_from = 'Cell_formatted',
    values_fill = ''
  ) %>%
  select(
    'Year',
    "Susceptible population_Min.",
    "Susceptible population_Max.",
    "Infectious population_Min.",
    "Infectious population_Max.",
    "Positive pathogen tests_Min.",
    "Positive pathogen tests_Max.",
    "Positive antibody tests_Min.",
    "Positive antibody tests_Max."
  ) %>%
  arrange(
    Year
  ) %>%
  setNames(gsub(".*_", "", names(.))) %>%
  kable(align = 'r') %>%
  kable_styling("striped") %>%
  add_header_above(header_names) %>%
  add_header_above(superheader_names)

#
# print condensed wide version of almanac table
#

superheader_names = c(
  '',
  'Population' = 3,
  'Positive tests' = 2
)

sibr_metrics %>% 
  mutate(
    Year = year(Date)
  ) %>% 
  group_by(
    Year, Metric
  ) %>% 
  summarise(
    Range = paste0(
      paste(round(range(Percentage*100)), collapse = '-'), 
      '%'
    )
  ) %>% 
  pivot_wider(
    id_cols = Year,
    names_from = Metric, 
    values_from = Range
  ) %>% 
  arrange(
    Year
  ) %>% 
  select(
    Susceptible = 'Susceptible population',
    Infectious = 'Infectious population',
    Recovered = 'Recovered population',
    Pathogen = 'Positive pathogen tests',
    Antibody = 'Positive antibody tests'
  ) %>% 
  kable(align = 'c') %>%
  kable_styling('striped') %>% 
  add_header_above(superheader_names)
