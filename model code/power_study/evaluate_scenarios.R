#
# set random seed for task relative to entire job
#

# total number of cores to split work across
max_tasks = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_COUNT'))

# task to process
taskId = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

# set seed for job
RNGkind("L'Ecuyer-CMRG")

# # local debugging
# max_tasks = 1e3
# taskId = 524

# set seed for task
s <- .Random.seed
for (i in 1:taskId) {
  s <- parallel::nextRNGStream(s)
}
.GlobalEnv$.Random.seed <- s

#
# set up workspace
#

library(RSQLite)
library(dplyr)

invisible(
  lapply(dir(path = 'utils', pattern = '*.R', full.names = TRUE), source)
)

#
# define power study scenarios to evaluate
#

sample_sizes = unique(c(
  seq(from = 10, to = 100, by = 10), 
  seq(from = 100, to = 500, by = 50)
))

prevalence_priors = rbind(
  c(1, 1),
    data.frame(rbind(
    t(sapply(
      X = seq(from = 0, to = .7, by = .1), 
      FUN = function(lwr) beta_params(lower = lwr, upper = lwr + .3)
    ))
  ))
)

# tasks to process
target_tasks = expand.grid(
  prior_prevalence = 1:nrow(prevalence_priors),
  prior_disease = seq(from = .1, to = 1, by = .1),
  sensitivity = seq(from = .5, to = 1, by = .05),
  specificity = seq(from = .5, to = 1, by = .05),
  true_prevalence = seq(from = 0, to = 1, by = .05),
  freedom_signif = .05,
  disease_signif = .05,
  prevalence_conf = .95,
  prevalence_quantile = .95
) %>% 
  mutate(
    task = 1:n()
  ) %>% 
  filter(
    task %in% parallel::splitIndices(nx = n(), ncl = max_tasks)[[taskId]]
  )

#
# connect to output storage
#

odir = file.path('output', 'power_study')

dir.create(path = odir, showWarnings = FALSE, recursive = TRUE)

# task-specific output file, for streaming output writing
f = file.path(odir, paste('task-', taskId, '.sqlite3', sep = ''))

# open connection to database; create db file if needed
con = dbConnect(SQLite(), dbname = f, flags = RSQLite::SQLITE_RWC)

output_table = 'power_study'

# initialize output table if needed
if(isFALSE(output_table %in% dbListTables(con))) {
  table_structure = structure(
    list(
      prior_prevalence = integer(0), prior_disease = numeric(0), 
      sensitivity = numeric(0), specificity = numeric(0), 
      true_prevalence = numeric(0), freedom_signif = numeric(0),
      disease_signif = numeric(0), 
      prevalence_conf = numeric(0), prevalence_quantile = numeric(0), 
      task = integer(0), sample_size = numeric(0), 
      prob_signif_disease_freedom = numeric(0),
      prob_signif_disease = numeric(0), 
      prob_prevalence_hpd_covers = numeric(0), 
      prob_prevalence_bounded = numeric(0), 
      mean_prevalence_ci_width = numeric(0), mean_prevalence_bound = numeric(0), 
      mean_prevalence_cv = numeric(0), 
      time = structure(numeric(0), class = "difftime", units = "secs")
    ), 
    row.names = integer(0), 
    class = "data.frame"
  )
  dbWriteTable(conn = con, name = output_table, value = table_structure)
}

######################
# process task group #
######################

# loop over study configurations
for(subTaskId in 1:nrow(target_tasks)) {
  
  # isolate task
  subTask_details = target_tasks[subTaskId,]
  rownames(subTask_details) = NULL
  
  # evaluate power studies, looping over sample sizes
  subtask_results = cbind(
    subTask_details,
    do.call(rbind, lapply(sample_sizes, function(sample_size) {
      tick = Sys.time()
      res = cbind(
        sample_size = sample_size, 
        presence_prevalence_power(
          n = sample_size, 
          prior_prevalence = prevalence_priors[subTask_details$prior_prevalence,], 
          prior_disease = subTask_details$prior_disease, 
          sensitivity = subTask_details$sensitivity, 
          specificity = subTask_details$specificity, 
          true_prevalence = subTask_details$true_prevalence,
          freedom_signif = subTask_details$freedom_signif,
          disease_signif = subTask_details$disease_signif, 
          prevalence_conf = subTask_details$prevalence_conf, 
          prevalence_quantile = subTask_details$prevalence_quantile
        )
      )
      tock = Sys.time()
      res$time = tock - tick
      res
    }))
  )
  
  # save results
  dbAppendTable(conn = con, name = output_table, value = subtask_results)
}

dbDisconnect(con)
