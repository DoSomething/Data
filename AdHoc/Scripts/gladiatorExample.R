source('Scripts/init.R')
source('Scripts/DSUtils.R')

comp <- tbl_dt(read.csv('Data/contest_28-competition_209-users.csv'))
qres <- tbl_dt(read.csv('Data/query_result (5).csv'))

combine <-
  qres %>%
  left_join(comp, by = 'Northstar.ID') %>%
  mutate(
    reportback_for_contest = ifelse(is.na(Number.of.Posts) | Number.of.Posts == 0, 0, Number.of.Posts),
    reportback = ifelse(is.na(Number.of.Posts) | Number.of.Posts == 0, 0, 1)
  ) %>%
  filter(
    !is.na(Email)
  ) %>%
  summarise(
    avgNumberPosts = mean(Number.of.Posts),
    reportbackRate = mean(reportback)
  )

write.csv(combine, 'Data/output.csv')

####
qres <-
  read.csv('~/Desktop/query_result.csv') %>% tbl_dt()

sosRB <-
  rbind(
    read_csv("~/Downloads/Steps for SoldiersContest 45_Run 7827 Hasn't Reported Back/contest_45-competition_319-users.csv"),
    read_csv("~/Downloads/Steps for SoldiersContest 45_Run 7827 Hasn't Reported Back/contest_45-competition_325-users.csv"),
    read_csv("~/Downloads/Steps for SoldiersContest 45_Run 7827 Hasn't Reported Back/contest_45-competition_327-users.csv")
  ) %>%
  dplyr::rename(Northstar.ID = `Northstar ID`) %>%
  mutate(campaign_run_id = 7827)
  
RB <- 
  qres %>%
  left_join(sosRB, copy=T) %>%
  mutate(
    reportback = ifelse(number_of_posts > 0, 1, 0)
  ) %>%
  group_by(campaign_run_id) %>%
  summarise(
    Reportbacks = sum(reportback),
    reportbackRate = mean(reportback)
  ) %>% print()

sosMetr <- 
  rbind(
    read_csv('~/Downloads/Steps for SoldiersContest 45_Run 7827 Competition Level Metrics/contest_45-competition_314-users.csv'),
    read_csv('~/Downloads/Steps for SoldiersContest 45_Run 7827 Competition Level Metrics/contest_45-competition_316-users.csv')
  ) %>%
  mutate(campaign_run_id = 7827)
gladMetr <- 
  rbind(
    read_csv('~/Downloads/Treat Yo Friends Contest 47_Run 7873 Competition Level Metrics/contest_47-competition_326-users.csv'),
    read_csv('~/Downloads/Treat Yo Friends Contest 47_Run 7873 Competition Level Metrics/contest_47-competition_329-users.csv'),
    read_csv('~/Downloads/Treat Yo Friends Contest 47_Run 7873 Competition Level Metrics/contest_47-competition_330-users.csv') 
  ) %>%
  mutate(campaign_run_id = 7873) %>%
  rbind(sosMetr) %>% tbl_dt() %>%
  dplyr::rename(Northstar.ID = `Northstar ID`)

compOut <- 
  qres %>%
  left_join(gladMetr) %>%
  filter(number_of_posts == 0) %>%
  select(Northstar.ID, Email, `First Name`, `Last Name`, number_of_posts)

saveCSV(compOut, desktop = T)

####