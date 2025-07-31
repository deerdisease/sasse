library(deSolve)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(dplyr)
library(ggpubr)
library(lubridate)

#
# user input
#

# epi parameters
avg_infectious_days_input = 7
avg_recovery_days_input = 30
avg_waning_days_input = 180
R0 = 1.5

# initial population sizes at t=0
initial_susceptible_members = 1e3
initial_infectious_members = 1
initial_broadly_recovered_members = 0
initial_recovered_members = 0

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
  # proportion = c(.4) 
  proportion = 0
)
# birth_pulses = NULL

#
# solve model
#

# initial proportions at t=0
total_population = initial_susceptible_members + initial_infectious_members + 
  initial_broadly_recovered_members + initial_recovered_members
initial_susceptible = initial_susceptible_members / total_population
initial_infectious = initial_infectious_members / total_population
initial_broadly_recovered = initial_broadly_recovered_members / total_population
initial_recovered = initial_recovered_members / total_population

# threshold below which there are no infectious individuals in the population
minimum_infectious_proportion = 1 / total_population

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
      incremental_solution = data.frame(
        ode(
          y = initial_values,
          times = t,
          func = sibr_equations,
          parms = parameter_values
        )[-1,]
      )
      # re-solve, as needed if disease leaves population
      if(any(incremental_solution$i < minimum_infectious_proportion)) {
        # identify when infection ends (i.e., 0 infected individuals)
        infection_end_index = min(which(
          incremental_solution$i < minimum_infectious_proportion
        ))
        # formally remove infection
        incremental_solution$i[infection_end_index] = 0
        mass = sum(
          incremental_solution[infection_end_index, c('s', 'i', 'b', 'r')]
        )
        incremental_solution[infection_end_index, c('s', 'i', 'b', 'r')] = 
          incremental_solution[infection_end_index, c('s', 'i', 'b', 'r')] / 
          mass
        # re-solve sibr equations for post-disease state
        resolve_inds = infection_end_index:nrow(incremental_solution)
        incremental_solution[resolve_inds, ] = data.frame(
          ode(
            y = unlist(
              incremental_solution[infection_end_index, c('s', 'i', 'b', 'r')]
            ),
            times = incremental_solution$time[resolve_inds],
            func = sibr_equations,
            parms = parameter_values
          )
        )
      }
      # append solution
      sibr_solution = rbind(
        sibr_solution,
        incremental_solution
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
  # re-solve, as needed if disease leaves population
  if(any(sibr_solution$i < minimum_infectious_proportion)) {
    # identify when infection ends (i.e., 0 infected individuals)
    infection_end_index = min(which(
      sibr_solution$i < minimum_infectious_proportion
    ))
    # formally 0-out infection
    sibr_solution$i[infection_end_index] = 0
    mass = sum(
      sibr_solution[infection_end_index, c('s', 'i', 'b', 'r')]
    )
    sibr_solution[infection_end_index, c('s', 'i', 'b', 'r')] = 
      sibr_solution[infection_end_index, c('s', 'i', 'b', 'r')] / 
      mass
    # re-solve sibr equations for post-disease state
    resolve_inds = infection_end_index:nrow(sibr_solution)
    sibr_solution[resolve_inds, ] = data.frame(
      ode(
        y = unlist(
          sibr_solution[infection_end_index, c('s', 'i', 'b', 'r')]
        ),
        times = sibr_solution$time[resolve_inds],
        func = sibr_equations,
        parms = parameter_values
      )
    )
  }
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


