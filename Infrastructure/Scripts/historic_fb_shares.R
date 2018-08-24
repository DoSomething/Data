source('config/init.R')
source('config/pgConnect.R')
library(glue)
library(openxlsx)
pg <- pgConnect()

fbShare <-
  runQuery('Scripts/historic_fbshare_export.sql')

lookup <- read.xlsx('Data/id_run_lookup.xlsx')

shares <-
  fbShare %>%
  filter(
    !is.na(user.northstarId) &
    user.northstarId != '' &
    !grepl('user.id', user.northstarId) &
    !grepl('NSID', user.northstarId)
    ) %>%
  group_by(user.northstarId, data.legacyCampaignId) %>%
  mutate(
    action =
      paste0('action-',cumsum(data.url != lag(data.url, default = '')))
  ) %>%
  left_join(lookup)

saveCSV(shares)
