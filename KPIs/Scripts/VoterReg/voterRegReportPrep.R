library(stringr)
source('Scripts/VoterReg/turbovoteFile.R')
source('Scripts/VoterReg/rockTheVoteFile.R')
source('Scripts/VoterReg/schoolTheVote.R')

library(reshape2)

vr <-
  rtv %>%
  bind_rows(tv)

if(dbExistsTable(pg,c("public", "turbovote_file"))) {

  q <- "truncate public.turbovote_file"
  runQuery(q)

}
dbWriteTable(pg,c("public", "turbovote_file"), vr, append = TRUE, row.names=F)

vr <-
  rtv %>%
  bind_rows(tv) %>%
  bind_rows(stv)

npPivot <- function(pivot) {

  pivot <- enquo(pivot)
  out <-
    vr %>%
    filter(!is.na(!!pivot) & (!!pivot)!='') %>%
    mutate(
      status = ifelse(grepl('register', ds_vr_status), 'registered', ds_vr_status)
    ) %>%
    group_by(status, !!pivot) %>%
    summarise(Count=n()) %>%
    mutate(Proportion=Count/sum(Count))  %>%
    melt(value.var='Proportion') %>% as.tibble() %>%
    mutate(
      label = case_when(
        variable=='Count' ~ as.character(value),
        TRUE ~ paste0(round(value*100,1),'%')
      )
    )
  return(out)
}

uSource <- npPivot(user_source)
Source <- npPivot(source)
detSource <- npPivot(source_details)

sourceTime <-
  vr %>%
  mutate(date=as.Date(created_at)) %>%
  filter(date>'2017-09-27') %>%
  group_by(date, source) %>%
  summarise(
    reg=length(which(grepl('register', ds_vr_status)))
  ) %>%
  group_by(source) %>%
  mutate(
    runningTotal = cumsum(reg)
  )

fileSource <-
  vr %>%
  filter(grepl('register', ds_vr_status)) %>%
  group_by(file) %>%
  count() %>%
  ungroup() %>%
  arrange(n) %>%
  mutate(
    file = factor(file, levels=c('RockTheVote','TurboVote','OnTheGround'))
  )

sourceStep <-
  vr %>%
  filter(source != '') %>%
  mutate(
    status = ifelse(grepl('register', ds_vr_status), 'registered', ds_vr_status)
  ) %>%
  group_by(status, source, details) %>%
  summarise(Count=n()) %>%
  mutate(Proportion=Count/sum(Count)) %>%
  melt(value.var='Proportion') %>% as.tibble() %>%
  mutate(
    label = case_when(
      variable=='Count' ~ as.character(value),
      TRUE ~ paste0(round(value*100,1),'%')
    )
  )

## For Pacing Doc

byWeek <-
  vr %>%
  group_by(week) %>%
  summarise(
    tot_vot_reg = grepl('register', ds_vr_status) %>% sum(),
    rbs = sum(reportback),
    complete_form = grepl('form', ds_vr_status) %>% sum(),
    complete_online = grepl('OVR', ds_vr_status) %>% sum(),
    self_report = sum(ds_vr_status=='confirmed')
  )

byWeekSource <-
  vr %>%
  group_by(week, source) %>%
  summarise(
    tot_vot_reg = grepl('register', ds_vr_status) %>% sum()
  ) %>%
  melt(value.var = c('tot_vot_reg')) %>%
  dcast(week ~ source + variable, value.var='value') %>%
  replace(is.na(.), 0)

byWeekSourceDetails <-
  vr %>%
  group_by(week, source, source_details) %>%
  summarise(
    tot_vot_reg = grepl('register', ds_vr_status) %>% sum(),
    rbs = sum(reportback),
    complete_form = grepl('form', ds_vr_status) %>% sum(),
    complete_online = grepl('OVR', ds_vr_status) %>% sum(),
    self_report = sum(ds_vr_status=='confirmed')
  )

## For Asterisks Doc

aster <-
  vr %>%
  mutate(
    campaign_id =
      case_when(
        grepl('campaign',campaign_id) | campaign_id %in% c('','0') ~ '8017',
        TRUE ~ campaign_id
      )
  ) %>%
  group_by(month, campaign_id) %>%
  summarise(
    rbs = sum(reportback),
    tot_vot_reg = grepl('register', ds_vr_status) %>% sum(),
    self_report = sum(ds_vr_status=='confirmed')
  )

## Month over month view

MoM <-
  vr %>%
  filter(created_at >= '2018-01-01') %>%
  mutate(
    date = as.Date(created_at)
  ) %>%
  group_by(date) %>%
  summarise(
    Registrations = length(which(grepl('register', ds_vr_status)))
  ) %>%
  mutate(
<<<<<<< HEAD
    Month = months(date)
=======
    month = months(date)
>>>>>>> 2751445b400e6958ada2d9568f7244974aea574b
  ) %>%
  group_by(Month) %>%
  mutate(
    registerToDate = cumsum(Registrations),
    dayOfMonth = as.numeric(format(date, "%d"))
  ) %>%
  ungroup() %>%
  select(dayOfMonth, Month, registerToDate) %>%
  melt(id.var=c('dayOfMonth','Month')) %>%
  mutate(Month = factor(Month, levels=month.name))

# By Quarter and Source

QoQ.Source <-
  vr %>%
  filter(created_at >= '2018-01-01') %>%
  mutate(
    date = as.Date(created_at)
    ) %>%
  group_by(date, source) %>%
  summarise(
    Registrations = length(which(grepl('register', ds_vr_status)))
  ) %>%
  mutate(
    quarter = quarter(date)
  ) %>%
  group_by(quarter, source) %>%
  mutate(
    registerToDate = cumsum(Registrations),
    dayOfQuarter = case_when(
      quarter==1 ~ yday(date),
      quarter==2 ~ yday(date)-90,
      quarter==3 ~ yday(date)-180,
      quarter==4 ~ yday(date)-270
    )
  ) %>%
  ungroup() %>%
  select(dayOfQuarter, quarter, registerToDate, source) %>%
  melt(id.var=c('dayOfQuarter','quarter','source')) %>%
  mutate(quarter = as.factor(quarter))


QoQ <-
  vr %>%
  filter(created_at >= '2018-01-01') %>%
  mutate(
    date = as.Date(created_at)
  ) %>%
  group_by(date) %>%
  summarise(
    Registrations = length(which(grepl('register', ds_vr_status)))
  ) %>%
  mutate(
    quarter = quarter(date)
  ) %>%
  group_by(quarter) %>%
  mutate(
    registerToDate = cumsum(Registrations),
    dayOfQuarter = case_when(
      quarter==1 ~ yday(date),
      quarter==2 ~ yday(date)-90,
      quarter==3 ~ yday(date)-180,
      quarter==4 ~ yday(date)-270
    )
  ) %>%
  ungroup() %>%
  select(dayOfQuarter, quarter, registerToDate) %>%
  melt(id.var=c('dayOfQuarter','quarter')) %>%
  mutate(quarter = as.factor(quarter))


# Excel output

library(openxlsx)

raw <- createWorkbook()
wb <- createWorkbook()

addWorksheet(raw, 'rawData')
writeData(raw, 'rawData', vr, rowNames = F)
addWorksheet(wb, 'byWeek')
writeData(wb, 'byWeek', byWeek, rowNames=F)
addWorksheet(wb, 'byWeekSource')
writeData(wb, 'byWeekSource', byWeekSource, rowNames=F)
addWorksheet(wb, 'byWeekSourceDetails')
writeData(wb, 'byWeekSourceDetails', byWeekSourceDetails, rowNames=F)
addWorksheet(wb, 'RBAsterisk')
writeData(wb, 'RBAsterisk', aster, rowNames=F)

saveWorkbook(
  raw,
  paste0('Data/raw_output_',Sys.Date(),'.xlsx'),
  overwrite = TRUE
)

saveWorkbook(
  wb,
  paste0('Data/aggregate_output_',Sys.Date(),'.xlsx'),
  overwrite = TRUE
)
