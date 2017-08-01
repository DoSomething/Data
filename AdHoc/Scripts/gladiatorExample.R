source('Scripts/init.R')
source('Scripts/DSUtils.R')


# Competition Metrics -----------------------------------------------------

qres <-
  read.csv('~/Desktop/query result.csv') %>% tbl_dt()

sosRB <-
  rbind(
    rbind(
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/StepsForSoldiers_RunID7827_CompetitionStats_7.31_Export/contest_45-competition_314-users.csv")%>%mutate(competition=314),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/StepsForSoldiers_RunID7827_CompetitionStats_7.31_Export/contest_45-competition_316-users.csv")%>%mutate(competition=316),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/StepsForSoldiers_RunID7827_CompetitionStats_7.31_Export/contest_45-competition_319-users.csv")%>%mutate(competition=319),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/StepsForSoldiers_RunID7827_CompetitionStats_7.31_Export/contest_45-competition_325-users.csv")%>%mutate(competition=325),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/StepsForSoldiers_RunID7827_CompetitionStats_7.31_Export/contest_45-competition_327-users.csv")%>%mutate(competition=327),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/StepsForSoldiers_RunID7827_CompetitionStats_7.31_Export/contest_45-competition_334-users.csv")%>%mutate(competition=334),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/StepsForSoldiers_RunID7827_CompetitionStats_7.31_Export/contest_45-competition_337-users.csv")%>%mutate(competition=337),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/StepsForSoldiers_RunID7827_CompetitionStats_7.31_Export/contest_45-competition_340-users.csv")%>%mutate(competition=340),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/StepsForSoldiers_RunID7827_CompetitionStats_7.31_Export/contest_45-competition_344-users.csv")%>%mutate(competition=344)
    ) %>% mutate(campaign_run_id = 7827, contest=45)
    ,
    rbind(
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/ThumbWars_RunID7811_CompetitionStats_7.31_Export/contest_44-competition_292-users.csv")%>%mutate(competition=292),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/ThumbWars_RunID7811_CompetitionStats_7.31_Export/contest_44-competition_294-users.csv")%>%mutate(competition=294),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/ThumbWars_RunID7811_CompetitionStats_7.31_Export/contest_44-competition_299-users.csv")%>%mutate(competition=299),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/ThumbWars_RunID7811_CompetitionStats_7.31_Export/contest_44-competition_302-users.csv")%>%mutate(competition=302),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/ThumbWars_RunID7811_CompetitionStats_7.31_Export/contest_44-competition_307-users.csv")%>%mutate(competition=307),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/ThumbWars_RunID7811_CompetitionStats_7.31_Export/contest_44-competition_311-users.csv")%>%mutate(competition=311),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/ThumbWars_RunID7811_CompetitionStats_7.31_Export/contest_44-competition_317-users.csv")%>%mutate(competition=317),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/ThumbWars_RunID7811_CompetitionStats_7.31_Export/contest_44-competition_320-users.csv")%>%mutate(competition=320),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/ThumbWars_RunID7811_CompetitionStats_7.31_Export/contest_44-competition_324-users.csv")%>%mutate(competition=324),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/ThumbWars_RunID7811_CompetitionStats_7.31_Export/contest_44-competition_328-users.csv")%>%mutate(competition=328),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/ThumbWars_RunID7811_CompetitionStats_7.31_Export/contest_44-competition_333-users.csv")%>%mutate(competition=333),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/ThumbWars_RunID7811_CompetitionStats_7.31_Export/contest_44-competition_338-users.csv")%>%mutate(competition=338),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/ThumbWars_RunID7811_CompetitionStats_7.31_Export/contest_44-competition_342-users.csv")%>%mutate(competition=342),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/ThumbWars_RunID7811_CompetitionStats_7.31_Export/contest_44-competition_345-users.csv")%>%mutate(competition=345)
    ) %>% mutate(campaign_run_id = 7811, contest=44)
    ,
    rbind(
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/TreatYoFriends_RunID7877_CompetitionStats_7.31_Export/contest_48-competition_335-users.csv")%>%mutate(competition=335),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/TreatYoFriends_RunID7877_CompetitionStats_7.31_Export/contest_48-competition_336-users.csv")%>%mutate(competition=336),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/TreatYoFriends_RunID7877_CompetitionStats_7.31_Export/contest_48-competition_339-users.csv")%>%mutate(competition=339),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/TreatYoFriends_RunID7877_CompetitionStats_7.31_Export/contest_48-competition_341-users.csv")%>%mutate(competition=341),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/TreatYoFriends_RunID7877_CompetitionStats_7.31_Export/contest_48-competition_343-users.csv")%>%mutate(competition=343)
    ) %>% mutate(campaign_run_id = 7877, contest=48)
    ,
    rbind(
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/TreatYoFriends_RunID7825_CompetitionStats_7.31_Export/contest_46-competition_315-users.csv")%>%mutate(competition=315),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/TreatYoFriends_RunID7825_CompetitionStats_7.31_Export/contest_46-competition_318-users.csv")%>%mutate(competition=318),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/TreatYoFriends_RunID7825_CompetitionStats_7.31_Export/contest_46-competition_321-users.csv")%>%mutate(competition=321),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/TreatYoFriends_RunID7825_CompetitionStats_7.31_Export/contest_46-competition_322-users.csv")%>%mutate(competition=322),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/TreatYoFriends_RunID7825_CompetitionStats_7.31_Export/contest_46-competition_323-users.csv")%>%mutate(competition=323)
    ) %>% mutate(campaign_run_id = 7825, contest=46)
    ,
    rbind(
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/TreatYoFriends_RunID7873_CompetitionStats_7.31_Export/contest_47-competition_326-users.csv")%>%mutate(competition=326),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/TreatYoFriends_RunID7873_CompetitionStats_7.31_Export/contest_47-competition_329-users.csv")%>%mutate(competition=329),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/TreatYoFriends_RunID7873_CompetitionStats_7.31_Export/contest_47-competition_330-users.csv")%>%mutate(competition=330),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/TreatYoFriends_RunID7873_CompetitionStats_7.31_Export/contest_47-competition_331-users.csv")%>%mutate(competition=331),
      read_csv("~/Downloads/GladiatorMetrics/Metrics/Run731/TreatYoFriends_RunID7873_CompetitionStats_7.31_Export/contest_47-competition_332-users.csv")%>%mutate(competition=332)
    ) %>% mutate(campaign_run_id = 7873, contest=47)
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


# No Reportback -----------------------------------------------------------

gladMetr <- 
  rbind(
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/StepsForSoldiers_RunID7827_NoRB_7.28_Export/contest_45-competition_316-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/StepsForSoldiers_RunID7827_NoRB_7.28_Export/contest_45-competition_319-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/StepsForSoldiers_RunID7827_NoRB_7.28_Export/contest_45-competition_325-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/StepsForSoldiers_RunID7827_NoRB_7.28_Export/contest_45-competition_327-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/StepsForSoldiers_RunID7827_NoRB_7.28_Export/contest_45-competition_334-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/StepsForSoldiers_RunID7827_NoRB_7.28_Export/contest_45-competition_337-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/StepsForSoldiers_RunID7827_NoRB_7.28_Export/contest_45-competition_340-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/ThumbWars_RunID7811_NoRB_7.28_Export/contest_44-competition_317-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/ThumbWars_RunID7811_NoRB_7.28_Export/contest_44-competition_320-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/ThumbWars_RunID7811_NoRB_7.28_Export/contest_44-competition_324-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/ThumbWars_RunID7811_NoRB_7.28_Export/contest_44-competition_328-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/ThumbWars_RunID7811_NoRB_7.28_Export/contest_44-competition_333-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/ThumbWars_RunID7811_NoRB_7.28_Export/contest_44-competition_338-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/ThumbWars_RunID7811_NoRB_7.28_Export/contest_44-competition_342-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/TreatYoFriends_RunID7877_NoRB_7.28_Export/contest_48-competition_335-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/TreatYoFriends_RunID7877_NoRB_7.28_Export/contest_48-competition_336-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/TreatYoFriends_RunID7877_NoRB_7.28_Export/contest_48-competition_339-users.csv'),
    read_csv('~/Downloads/GladiatorMetrics/HasntReportBack/TreatYoFriends_RunID7877_NoRB_7.28_Export/contest_48-competition_341-users.csv')
  ) %>%
  tbl_dt() %>%
  dplyr::rename(Northstar.ID = `Northstar ID`, campaign_run_id = `Run ID`)

compOut <- 
  qres %>%
  inner_join(gladMetr) %>%
  filter(number_of_posts == 0) %>%
  select(Northstar.ID, Email, campaign_run_id, `First Name`, `Last Name`, number_of_posts)

saveCSV(compOut, desktop = T)

####


a=read_csv("~/Downloads/GladiatorMetrics/Metrics/StepsForSoldiers_RunID7827_CompetitionStats_7.25_Export/contest_45-competition_314-users.csv")
b=read_csv("~/Downloads/GladiatorMetrics/Metrics/StepsForSoldiers_RunID7827_CompetitionStats_7.31_Export/contest_45-competition_314-users.csv")







