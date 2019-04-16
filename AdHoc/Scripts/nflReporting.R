source('config/init.R')

signups <- runQuery('Scripts/nfl_signups.sql')
completions <- runQuery('Scripts/nfl_completions.sql')

write_csv(
  signups,
  paste0('Data/nfl_data/dosomething_completions_sweeps_',gsub('-','',today()),'.csv')
  )

write_csv(
  completions,
  paste0('Data/nfl_data/dosomething_signups_minutes_',gsub('-','',today()), '.csv')
)