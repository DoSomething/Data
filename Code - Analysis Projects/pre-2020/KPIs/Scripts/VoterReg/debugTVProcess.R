load('Data/Turbovote/tvProcessed_NEWLOGIC.RData')
load('Data/Turbovote/tvProcessed_OLDLOGIC.RData')

old=vr
new=out[[1]]

rm(vr, out)

new <- new %>% select(id, nsid, week, created_at, updated_at, ds_vr_status, reportback)
old <- old %>% select(id, nsid, week, created_at, updated_at, ds_vr_status, reportback)

com <- new %>% full_join(old, by = 'id')

oldweek <- old %>%
  group_by(week) %>%
  summarise(
    tot_vot_reg = grepl('register', ds_vr_status) %>% sum(),
    rbs = sum(reportback),
    complete_form = grepl('form', ds_vr_status) %>% sum(),
    complete_online = grepl('OVR', ds_vr_status) %>% sum(),
    self_report = sum(ds_vr_status=='confirmed')
  )

newweek <- new %>%
  group_by(week) %>%
  summarise(
    tot_vot_reg = grepl('register', ds_vr_status) %>% sum(),
    rbs = sum(reportback),
    complete_form = grepl('form', ds_vr_status) %>% sum(),
    complete_online = grepl('OVR', ds_vr_status) %>% sum(),
    self_report = sum(ds_vr_status=='confirmed')
  )
