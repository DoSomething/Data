source('Scripts/VoterReg/turbovoteFile.R')
source('Scripts/VoterReg/rockTheVoteFile.R')
source('Scripts/VoterReg/schoolTheVote.R')

library(reshape2)

vr <-
  rtv %>%
  bind_rows(tv)

if(dbExistsTable(pg,c("public", "turbovote_file"))) {

  q <- "truncate public.turbovote_file"
  runQuery(q,'pg')

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

fileSource <-
  vr %>%
  filter(grepl('register', ds_vr_status)) %>%
  group_by(file) %>%
  count() %>%
  ungroup() %>%
  arrange(-n) %>%
  mutate(
    pos = cumsum(n) - n/2
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

all <-
  vr %>%
  group_by(week) %>%
  summarise(
    tot_vot_reg = grepl('register', ds_vr_status) %>% sum(),
    rbs = sum(reportback),
    complete_form = grepl('form', ds_vr_status) %>% sum(),
    complete_online = grepl('OVR', ds_vr_status) %>% sum(),
    self_report = sum(ds_vr_status=='confirmed')
  )

bySource <-
  vr %>%
  group_by(week, source) %>%
  summarise(
    tot_vot_reg = grepl('register', ds_vr_status) %>% sum(),
    rbs = sum(reportback),
    complete_form = grepl('form', ds_vr_status) %>% sum(),
    complete_online = grepl('OVR', ds_vr_status) %>% sum(),
    self_report = sum(ds_vr_status=='confirmed')
  ) %>%
  melt(value.var =
         c('tot_vot_reg','rbs','complete_form',
           'complete_online','self_report')) %>%
  dcast(week ~ source + variable, value.var='value') %>%
  replace(is.na(.), 0) %>%
  select(week, starts_with('web'), starts_with('email'), starts_with('sms'),
         starts_with('social'),starts_with('part'), starts_with('no_attr'))

## For Asterisks Doc

aster <-
  vr %>%
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
    month = month(date)
  ) %>%
  group_by(month) %>%
  mutate(
    registerToDate = cumsum(Registrations),
    dayOfMonth = as.numeric(format(date, "%d"))
  ) %>%
  ungroup() %>%
  select(dayOfMonth, month, registerToDate) %>%
  melt(id.var=c('dayOfMonth','month')) %>%
  mutate(month = as.factor(month))

MoM.Source <-
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
    month = month(date)
  ) %>%
  group_by(month, source) %>%
  mutate(
    registerToDate = cumsum(Registrations),
    dayOfMonth = as.numeric(format(date, "%d"))
  ) %>%
  ungroup() %>%
  select(dayOfMonth, month, registerToDate, source) %>%
  melt(id.var=c('dayOfMonth','month','source')) %>%
  mutate(month = as.factor(month))

## Excel output

library(openxlsx)

wb <- createWorkbook()

addWorksheet(wb, 'rawData')
writeData(wb, 'rawData', vr, rowNames = F)
addWorksheet(wb, 'AllSources')
writeData(wb, 'AllSources', all, rowNames=F)
addWorksheet(wb, 'bySource')
writeData(wb, 'bySource', bySource, rowNames=F)
addWorksheet(wb, 'RBAsterisk')
writeData(wb, 'RBAsterisk', aster, rowNames=F)

saveWorkbook(
  wb,
  paste0('Data/Turbovote/output_',Sys.Date(),'.xlsx'),
  overwrite = TRUE
)
