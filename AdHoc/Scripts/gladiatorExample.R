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
    rbind(
      read_csv("~/Downloads/GladiatorMetrics/727Run/StepsForSoldiers_RunID7827_CompetitionStats_7.25_Export/contest_45-competition_314-users.csv")%>%mutate(competition=314),
      read_csv("~/Downloads/GladiatorMetrics/727Run/StepsForSoldiers_RunID7827_CompetitionStats_7.25_Export/contest_45-competition_316-users.csv")%>%mutate(competition=316),
      read_csv("~/Downloads/GladiatorMetrics/727Run/StepsForSoldiers_RunID7827_CompetitionStats_7.25_Export/contest_45-competition_319-users.csv")%>%mutate(competition=319),
      read_csv("~/Downloads/GladiatorMetrics/727Run/StepsForSoldiers_RunID7827_CompetitionStats_7.25_Export/contest_45-competition_325-users.csv")%>%mutate(competition=325),
      read_csv("~/Downloads/GladiatorMetrics/727Run/StepsForSoldiers_RunID7827_CompetitionStats_7.25_Export/contest_45-competition_327-users.csv")%>%mutate(competition=327),
      read_csv("~/Downloads/GladiatorMetrics/727Run/StepsForSoldiers_RunID7827_CompetitionStats_7.25_Export/contest_45-competition_334-users.csv")%>%mutate(competition=334),
      read_csv("~/Downloads/GladiatorMetrics/727Run/StepsForSoldiers_RunID7827_CompetitionStats_7.25_Export/contest_45-competition_337-users.csv")%>%mutate(competition=337)
    ) %>% mutate(campaign_run_id = 7827, contest=45)
    ,
    rbind(
      read_csv("~/Downloads/GladiatorMetrics/727Run/ThumbWars_RunID7811_CompetitionStats_7.25_Export/contest_44-competition_292-users.csv")%>%mutate(competition=292),
      read_csv("~/Downloads/GladiatorMetrics/727Run/ThumbWars_RunID7811_CompetitionStats_7.25_Export/contest_44-competition_294-users.csv")%>%mutate(competition=294),
      read_csv("~/Downloads/GladiatorMetrics/727Run/ThumbWars_RunID7811_CompetitionStats_7.25_Export/contest_44-competition_299-users.csv")%>%mutate(competition=299),
      read_csv("~/Downloads/GladiatorMetrics/727Run/ThumbWars_RunID7811_CompetitionStats_7.25_Export/contest_44-competition_302-users.csv")%>%mutate(competition=302),
      read_csv("~/Downloads/GladiatorMetrics/727Run/ThumbWars_RunID7811_CompetitionStats_7.25_Export/contest_44-competition_307-users.csv")%>%mutate(competition=307),
      read_csv("~/Downloads/GladiatorMetrics/727Run/ThumbWars_RunID7811_CompetitionStats_7.25_Export/contest_44-competition_311-users.csv")%>%mutate(competition=311),
      read_csv("~/Downloads/GladiatorMetrics/727Run/ThumbWars_RunID7811_CompetitionStats_7.25_Export/contest_44-competition_317-users.csv")%>%mutate(competition=317),
      read_csv("~/Downloads/GladiatorMetrics/727Run/ThumbWars_RunID7811_CompetitionStats_7.25_Export/contest_44-competition_320-users.csv")%>%mutate(competition=320),
      read_csv("~/Downloads/GladiatorMetrics/727Run/ThumbWars_RunID7811_CompetitionStats_7.25_Export/contest_44-competition_324-users.csv")%>%mutate(competition=324),
      read_csv("~/Downloads/GladiatorMetrics/727Run/ThumbWars_RunID7811_CompetitionStats_7.25_Export/contest_44-competition_328-users.csv")%>%mutate(competition=328),
      read_csv("~/Downloads/GladiatorMetrics/727Run/ThumbWars_RunID7811_CompetitionStats_7.25_Export/contest_44-competition_333-users.csv")%>%mutate(competition=333),
      read_csv("~/Downloads/GladiatorMetrics/727Run/ThumbWars_RunID7811_CompetitionStats_7.25_Export/contest_44-competition_338-users.csv")%>%mutate(competition=338)
    ) %>% mutate(campaign_run_id = 7811, contest=44)
    ,
    rbind(
      read_csv("~/Downloads/GladiatorMetrics/727Run/TreatYoFriends_RunID7877_CompetitionStats_7.24_Export/contest_48-competition_335-users.csv")%>%mutate(competition=335),
      read_csv("~/Downloads/GladiatorMetrics/727Run/TreatYoFriends_RunID7877_CompetitionStats_7.24_Export/contest_48-competition_336-users.csv")%>%mutate(competition=336),
      read_csv("~/Downloads/GladiatorMetrics/727Run/TreatYoFriends_RunID7877_CompetitionStats_7.24_Export/contest_48-competition_339-users.csv")%>%mutate(competition=339)
    ) %>% mutate(campaign_run_id = 7877, contest=48)
  ) %>%
  dplyr::rename(Northstar.ID = `Northstar ID`)
  
RB <- 
  sosRB %>%
  left_join(qres, copy=T) %>%
  mutate(
    reportback = ifelse(number_of_posts > 0, 1, 0)
  ) %>%
  filter(!is.na(number_of_posts)) %>%
  group_by(campaign_run_id) %>%
  summarise(
    Count = n(),
    Reportbacks = sum(reportback),
    reportbackRate = mean(reportback)
  ) %>% print()

gladMetr <- 
  rbind(
    read_csv('~/Downloads/727Run/TreatYoFriends_RunID7877_CompetitionStats_7.24_Export/contest_48-competition_335-users.csv'),
    read_csv('~/Downloads/727Run/TreatYoFriends_RunID7877_CompetitionStats_7.24_Export/contest_48-competition_336-users.csv')
  ) %>%
  tbl_dt() %>%
  dplyr::rename(Northstar.ID = `Northstar ID`)

compOut <- 
  qres %>%
  left_join(gladMetr) %>%
  filter(number_of_posts == 0) %>%
  select(Northstar.ID, Email, `First Name`, `Last Name`, number_of_posts)

saveCSV(compOut, desktop = T)

####