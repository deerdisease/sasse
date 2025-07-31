library(RSQLite)
library(dplyr)

#
# pull dataset
#

remote_host = 'host name'

remote_source = file.path('file path')

local_path = file.path(
  'output', 'power_study_merged', 
  paste(Sys.Date(), 'power_study_merged.sqlite3', sep = '-')
)

scp_user = 'user'

# download dataset
system(
  paste(
    'scp', 
    paste(
      paste(scp_user, remote_host, sep = '@'),
      remote_source,
      sep = ':'
    ),
    local_path
  )
)

#
# deduplicate dataset
#

con = dbConnect(SQLite(), dbname = local_path, flags = RSQLite::SQLITE_RO)

power_study = dbReadTable(con, 'power_study')

dbDisconnect(con)

# some of the power study results are duplicated or triplicated
power_study_deduplicated = power_study %>%
  group_by(task, sample_size) %>%
  slice(1) %>%
  ungroup()

#
# (re)write database
#

unlink(local_path)

con = dbConnect(SQLite(), dbname = local_path, flags = RSQLite::SQLITE_RWC)

target_table = 'power_study_deduplicated'

dbWriteTable(con, target_table, power_study_deduplicated)

#
# add index
#

dbListObjects(con)

dbExecute(
  conn = con, 
  statement = paste(
    'CREATE INDEX plotting_filter ON',
    target_table, 
    '(prior_prevalence, prior_disease, sensitivity, specificity, true_prevalence)'
  )
)

dbDisconnect(con)
