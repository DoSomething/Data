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
  ungroup() %>%
  left_join(lookup) %>%
  mutate(month = substr(to_timestamp, 1, 7))

monthList <- unique(shares$month)

for (j in 1:length(monthList)) {

  monthDat <-
    shares %>%
    filter(month == monthList[j])

  allSets <- split(monthDat, as.numeric(rownames(monthDat)) %/% 1200)

  for (i in 1:length(allSets)) {

    write_csv(
      allSets[[i]],
      path =
        paste0(
          'fbShares/shares_',
          monthList[j],
          '_',
          names(allSets[i]),
          '.csv'
        )
    )

  }

}
