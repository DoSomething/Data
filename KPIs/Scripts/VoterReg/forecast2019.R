library(stringr)
library(openxlsx)
source('Scripts/VoterReg/rogueTVRTV.R')

sources <-
  read_csv('Data/2019VR_Sources.csv') %>%
  distinct()

fordat <-
  tvrtv %>%
  filter(
    grepl('register',ds_vr_status) &
      source_details %in% sources$source_details
    ) %>%
  mutate(
    strategy =
      case_when(
        grepl('nvrd', tolower(source_details)) |
          source_details == 'newsletter_1063' ~ 'NVRD',
        grepl('bday', tolower(source_details)) ~ 'BdayEmail',
        grepl('hellobar', tolower(source_details)) ~ 'HelloBar',
        grepl('newsletter_', source_details) ~ 'LocalElections'
      ),
    date = as.Date(created_at)
  ) %>%
  group_by(date, strategy) %>%
  summarise(
    Registrations = n()
  )

NVRDTotal <-
  fordat %>%
  ungroup() %>%
  filter(strategy=='NVRD') %>%
  summarise(nvrdReg = sum(Registrations)) %>%
  bind_cols(
    fordat %>% ungroup() %>% filter(Registrations==max(Registrations)) %>% select(date)
  )

ggplot(fordat, aes(x=date, y=Registrations, group=strategy, color=strategy)) +
  geom_line(size=.5) +
  geom_smooth(color='black', size=.75) +
  facet_wrap(~strategy, scale='free_y', ncol=1)

dateSeq <-
  tibble(
    date = rep(seq.Date(as.Date('2018-01-01'), as.Date('2018-12-31'), 'days'), 2),
    doy = rep(seq(1,365,1), 2),
    strategy = rep(c('HelloBar','BdayEmail'), 365)
  )

modDat <-
  fordat %>%
  filter(strategy %in% c('HelloBar','BdayEmail')) %>%
  mutate(doy = yday(date))

hbMod <- loess(Registrations ~ doy, filter(modDat, strategy=='HelloBar'))
bdeMod <- loess(Registrations ~ doy, filter(modDat, strategy=='BdayEmail'))

predictDat <-
  dateSeq %>%
  left_join(modDat)

tempHB <-
  predictDat %>%
  filter(strategy=='HelloBar') %>%
  mutate(
    predReg = predict(hbMod, ., type='response')
    ) %>%
  select(date, doy, hellobarReg = Registrations, predReg.HB=predReg)
tempBD <-
  predictDat %>%
  filter(strategy=='BdayEmail') %>%
  mutate(
    predReg = predict(bdeMod, ., type='response')
  ) %>%
  select(date, doy, bdayReg = Registrations, predReg.BD=predReg)

predictDat <-
  tempHB %>%
  left_join(tempBD) %>%
  left_join(NVRDTotal) %>%
  left_join(
    fordat %>% filter(strategy=='LocalElections') %>% select(date, localReg = Registrations)
  ) %>%
  replace(is.na(.), 0)

predictAgg <-
  predictDat %>%
  mutate(quarter = quarter(date)) %>%
  group_by(quarter) %>%
  summarise(
    totalPredictpredReg.HB = sum(predReg.HB),
    totalPredictReg.BD = sum(predReg.BD),
    totalPredictReg.NVRD = sum(nvrdReg),
    totalPredict.LE = sum(localReg),
    totalRegistrations = sum(hellobarReg+bdayReg+nvrdReg+localReg)
  ) %>%
  mutate(
    totalPredictReg.NVRD = applyPctChange(totalPredictReg.NVRD, -.57),
    totalPredict.LE = applyPctChange(totalPredict.LE, -.57),
    totalPredictReg.BD = applyPctChange(totalPredictReg.BD, -.57),
    totalPredict = totalPredictReg.NVRD+totalPredict.LE+totalPredictReg.BD+totalPredictpredReg.HB
  )
