source('config/init.R')
source('config/mySQLConfig.R')
source('Q4/Scripts/customFunctions.R')
library(scales)
library(plotly)

nps.q4 <- mungeNPSQ4()
a=prepQueryObjects(nps.q4$northstar_id)

q <- paste0("
        SELECT DISTINCT
          ca.northstar_id,
          ca.signup_id,
          ci.*
        FROM quasar.campaign_activity ca
        LEFT JOIN
          (SELECT
            i.campaign_run_id,
            max(i.campaign_node_id_title) as campaign,
            max(i.campaign_cause_type) as 'cause_type',
            max(i.campaign_action_type) as'action_type'
          FROM quasar.campaign_info i
          GROUP BY i.campaign_run_id) ci
        ON ca.campaign_run_id = ci.campaign_run_id
        WHERE ca.northstar_id IN", a)

dat <-
  runQuery(q, which='mysql') %>%
  mutate(
    campaign = gsub("[^[:alnum:]]", "", campaign)
  )

High_Medium <-
  c('LoveLettersChallenge','SincerelyUs','TeensforJeans','5CansChallenge',
    'ThanksaBillion','WorldRecycleWeekCloseTheLoop','BirthdayMail',
    'DoSomethingAboutGunViolence','GrandparentsGoneWired',
    'MissinginHistory','GameWinningDrive','IHeartDad','GiveASpitAboutCancerShare')
Low <- c('ThumbWars','MirrorMessages','DontBeaSucker','AllInWeWin',
         'RideSeek','ScienceSleuth','SuperStressFace','ShowStressWhosBoss',
         'WhoHasTheirEyeonYou','IBeatBullying','MyBigRegret','ShowerSongs',
         'DefendDreamers','NewYearNewUS','LoseYourVCard','FindYourVSpot',
         'TreatYoFriends')

topCamp <-
  dat %>%
  group_by(campaign) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  filter(campaign %in% c(High_Medium, Low))

campaignList <- unique(topCamp$campaign)
percentDidCampaign <- data.frame()
for (i in 1:length(campaignList)) {
  temp <- percentDid(campaignList[i], dat)
  percentDidCampaign <- rbind(percentDidCampaign, temp)
}

uncastCombine <-
  nps.q4 %>%
  left_join(dat)
npsCampaign <- data.frame()
for (i in 1:length(campaignList)) {
  temp <- npsDidCampaign(uncastCombine, campaignList[i])
  npsCampaign <- rbind(npsCampaign, temp)
}
npsCampaign %>% arrange(-nps)
castAction <-
  nps.q4 %>%
  left_join(
    dat %>%
      filter(campaign %in% topCamp$campaign) %>%
      mutate(
        campaign = gsub("[^[:alnum:]]", "", campaign)
        ) %>%
      dplyr::select(northstar_id, campaign) %>%
      dcast(northstar_id ~ paste0('campaign_',campaign))
  )

nameList <- castAction %>% select(starts_with('campaign')) %>% colnames()
campaignImpact <- data.table()
p <- c()
for (i in 1:length(nameList)) {
  p[i] <- plotMod(nameList[i],castAction)
  campaignImpact <- rbind(campaignImpact, data.table(campaign=nameList[i], impact=p[i]))
}
campaignImpact[,campaign:=gsub('campaign_','',campaign)]

result <-
  as.tibble(campaignImpact) %>%
  left_join(percentDidCampaign) %>%
  left_join(npsCampaign) %>%
  arrange(-nps) %>%
  mutate(
    Barrier = ifelse(campaign %in% High_Medium, 'High/Medium',
                     ifelse(campaign %in% Low, 'Low', 'Other'))
  )
result$campaign <- factor(result$campaign, levels = result$campaign[order(result$Barrier,-result$nps)])

ggplot(result, aes(x=campaign, y=nps, fill=Barrier)) +
  geom_bar(stat='identity', position='dodge') + coord_flip()

nichePivot <-
  result %>%
  select(campaign, npsNiche, npsNonNiche, Barrier, countNiche, countNonNiche) %>%
  melt(value.var=c('nps','npsNiche','npsNonNiche','countNiche','countNonNiche'),
       value.name='nps', variable.names='Type')

ggplot(nichePivot, aes(x=campaign, y=nps, fill=Barrier)) +
  geom_bar(stat='identity', position='dodge') + coord_flip() +
  geom_text(aes(label=nps), size=3) +
  facet_wrap(~variable)

saveCSV(nichePivot, desktop=T)

aggFrame <-
  rbind(
    data.frame(Barrier='Low', nps=npsDidCampaign(uncastCombine, Low)),
    data.frame(Barrier='High/Medium', nps=npsDidCampaign(uncastCombine, High_Medium))
  )

saveCSV(result, desktop=T)


q <-
  "SELECT
    i.campaign_node_id_title AS campaign,
    COUNT(DISTINCT c.signup_id) AS signups
  FROM quasar.campaign_activity c
  LEFT JOIN quasar.campaign_info i
  ON i.campaign_run_id = c.campaign_run_id
  GROUP BY i.campaign_node_id_title"

res <- runQuery(q, 'mysql') %>%
  mutate(
    campaign = gsub("[^[:alnum:]]", "", campaign)
  )

campCount <-
  res %>%
  filter(campaign %in% c(High_Medium,Low)) %>%
  mutate(
    Barrier = ifelse(campaign %in% High_Medium, 'High/Medium',
                     ifelse(campaign %in% Low, 'Low', 'Other'))
  ) %>%
  group_by(Barrier) %>%
  summarise(Signups = sum(signups))

futureCal <- data.frame(NPS=c(20,2,6,5,30), q=c(1,5,6,7,11))
# futureUncal <- data.frame(NPS=c(29,6,8,5,30), q=c(1,3,4,5,9))
label <-
  c('Q2 2016','Q3 2016','Q4 2016','Q1 2017','Q2 2017',
    'Q3 2017','Q4 2017','Q1 2018','Q2 2018','Q3 2018',
    'Q4 2018')
ggplot(futureCal, aes(x=q, y=NPS, group=1)) +
  geom_point() + geom_line() + ylim(0,40) +
  scale_x_continuous(breaks=seq(1,11,1), name='Quarter',labels=label) +
  geom_text(aes(label=nps))
ggsave('NPS Goals.png', p, width = 10, height = 7)

ggplot(futureUncal, aes(x=q, y=NPS, group=1)) +
  geom_point() + geom_line() + ylim(0,40) +
  scale_x_continuous(
    breaks=seq(1,9,1), name='Quarter',
    labels=c('Q4 2016','Q1 2017','Q2 2017','Q3 2017',
             'Q4 2017','Q1 2018','Q2 2018','Q3 2018','Q3 2018')
  )
# pdf("plots.pdf")
# for (i in 1:length(p)) {
#   print(p[[i]])
# }
# dev.off()
