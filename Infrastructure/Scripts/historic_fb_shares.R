source('config/init.R')
source('config/pgConnect.R')
library(glue)
pg <- pgConnect()

fbShare <-
  runQuery('Scripts/historic_fbshare_export.sql')

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
  )

saveCSV(shares)
