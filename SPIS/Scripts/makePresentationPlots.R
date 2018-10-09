
# Demographics ------------------------------------------------------------
indexMark <- round(length(which(set$Group=='Members'))/nrow(set), 3)

genderComp <-
  sexGroup$Composition +
  ggtitle('Gender') +
  guides(fill=guide_legend(title="Gender"))
ggsave(plot = genderComp, 'Visuals/genderGroup.png', width = 7, height = 4)

ageComp <- stylePickOneList(set %>% filter(Group=='Members'), age, Group)
ageComp <-
  ageGroup$Frequency +
  ggtitle('Age') +
  scale_fill_brewer(palette='Set2',direction=-1) +
  scale_x_continuous(breaks=c(seq(13,25,1))) +
  coord_flip()
ggsave(plot = ageComp, 'Visuals/ageGroup.png', width = 7, height = 4)

raceComp <-
  raceGroup$Composition +
  ggtitle('Race') +
  guides(fill=guide_legend(title="Race"))
ggsave(plot = raceComp, 'Visuals/raceGroup.png', width = 7, height = 4)

regionComp <-
  regionGroup$Composition +
  ggtitle('Region') +
  guides(fill=guide_legend(title="Region"))
ggsave(plot=regionComp, 'Visuals/regionGroup.png', width = 7, height = 4)

financeComp <-
  financesGroup$Composition + ggtitle('Family Finances') +
  guides(fill=guide_legend(title="Family Finances"))
ggsave(plot=financeComp, 'Visuals/financeGroup.png', width = 7, height = 4)

parentComp <-
  parentEduGroup$Composition + ggtitle('Parental Education') +
  guides(fill=guide_legend(title="Education"))
ggsave(plot=parentComp, 'Visuals/parentEduGroup.png', width = 7, height = 4)

religiousComp <-
  religiousGroup$Composition + ggtitle('Religiosity') +
  guides(fill=guide_legend(title="Frequency"))
ggsave(plot=religiousComp, 'Visuals/religiousGroup.png', width = 7, height = 4)

partyComp <-
  partyGroup$Composition + ggtitle('Political Party') +
  guides(fill=guide_legend(title="Political Party"))
ggsave(plot=partyComp, 'Visuals/politicalPartyGroup.png', width = 7, height = 4)

politicsComp <-
  politicsGroup$Composition + ggtitle('Political Views') +
  guides(fill=guide_legend(title="Political View"))
ggsave(plot=politicsComp, 'Visuals/politicsGroup.png', width = 7, height = 4)

groupPartyView <-
  stylePickOneList(set, Group, political_party, political_view)$Facetted
ggsave(plot=groupPartyView, 'Visuals/politicsDive.png', width = 7, height = 4)

politicsAge <-
  stylePickOneList(set, age, political_party, Group)$Facetted +
  facet_wrap(~facet,ncol=1) +
  scale_x_continuous(breaks=seq(13,25,1)) + ggtitle('Political Party by Age') +
  guides(fill=guide_legend(title="Political View"))
ggsave(plot=politicsAge, 'Visuals/politicsAge.png', width = 8, height = 6)

genderRaceGroup <-
  stylePickOneList(set %>% filter(sex %in% c('Male','Female','Non-binary')), sex, race, Group)$Facetted +
  labs(title='Gender & Race Comparison') +
  guides(fill=guide_legend(title="Race"))
ggsave(plot=genderRaceGroup, 'Visuals/genderRaceGroup.png', width = 10, height = 6)
stylePickOneList(set %>% filter(Group=='Gen Pop' & sex %in% c('Male','Female','Non-binary')),
                 sex, race, Group)$Facetted +
  labs(title='Gender & Race Comparison') +
  guides(fill=guide_legend(title="Race"))

stylePickOneList(set %>% filter(sex %in% c('Male','Female','Non-binary')), Group, sex) +

# Brand -------------------------------------------------------------------

purchaseDecision <-
  bind_rows(
    tibble(
      Feature='Price',
      Value=weighted.mean(purchaseInfluence.Price$frequencyPlot$data$outcome,
                          purchaseInfluence.Price$frequencyPlot$data$count)
    ),
    tibble(
      Feature='Quality',
      Value=weighted.mean(purchaseInfluence.Quality$frequencyPlot$data$outcome,
                          purchaseInfluence.Quality$frequencyPlot$data$count)
    ),
    tibble(
      Feature='Celebrity Endorsement',
      Value=weighted.mean(purchaseInfluence.Celebrity$frequencyPlot$data$outcome,
                          purchaseInfluence.Celebrity$frequencyPlot$data$count)
    ),
    tibble(
      Feature='Company Values',
      Value=weighted.mean(purchaseInfluence.CompanyReputationValues$frequencyPlot$data$outcome,
                          purchaseInfluence.CompanyReputationValues$frequencyPlot$data$count)
    ),
    tibble(
      Feature='Customer Reviews',
      Value=weighted.mean(purchaseInfluence.CustomerReviews$frequencyPlot$data$outcome,
                          purchaseInfluence.CustomerReviews$frequencyPlot$data$count)
    ),
    tibble(
      Feature='Packaging',
      Value=weighted.mean(purchaseInfluence.Packaging$frequencyPlot$data$outcome,
                          purchaseInfluence.Packaging$frequencyPlot$data$count)
    ),
    tibble(
      Feature='Friend/Family Recommended',
      Value=weighted.mean(purchaseInfluence.RecommendedByKnownPerson$frequencyPlot$data$outcome,
                          purchaseInfluence.RecommendedByKnownPerson$frequencyPlot$data$count)
    )
  )

grid.arrange(
  purchaseInfluence.Celebrity$frequencyPlot,
  purchaseInfluence.CompanyReputationValues$frequencyPlot,
  purchaseInfluence.CustomerReviews$frequencyPlot,
  purchaseInfluence.Packaging$frequencyPlot,
  purchaseInfluence.Price$frequencyPlot,
  purchaseInfluence.Quality$frequencyPlot,
  purchaseInfluence.RecommendedByKnownPerson$frequencyPlot,
  ncol=3
)

purchaseDecision.p <-
  ggplot(purchaseDecision, aes(x=reorder(Feature, -Value), y=Value)) +
  geom_bar(stat='identity', fill='#6ac6b4') +
  scale_y_continuous(
    breaks=seq(-2,2,1), limits=c(-2,2),
    labels=c('Not Influential','A Little','Neutral','Influential','Very Influential')
    ) +
  labs(x='Influencer', y='How Influential',
       title='How Influential is Each of These Items When Purchasing a Product') +
  theme(plot.title = element_text(hjust=.5),
        axis.text.x = element_text(angle=30, hjust=1))
ggsave(plot=purchaseDecision.p, 'Visuals/purchaseDecision.png', width = 10, height = 6)

purchaseDecision.politics <-
  purchaseInfluence.CompanyReputationValues$pivotPlot[[1]] +
  labs(title = 'Pivoted by Political Views', y = 'How Influential') +
  theme(plot.title=element_text(hjust=.5, size = 10)) +
  scale_y_continuous(
    breaks=seq(-2,2,1), limits=c(-2,2),
    labels=c('Not Influential','A Little','Neutral','Influential','Very Influential')
  )

purchaseDecision.Gender <-
  purchaseInfluence.CompanyReputationValues$pivotPlot[[4]] +
  labs(title='Pivoted by Gender') +
  theme(plot.title=element_text(hjust=.5, size = 10)) +
  scale_y_continuous(
    breaks=seq(-2,2,1), limits=c(-2,2),
    labels=c('Not Influential','A Little','Neutral','Influential','Very Influential')
  )

grid.arrange(purchaseDecision.politics, purchaseDecision.Gender,
             top='Influence of Brand Values on Purchasing Decisions', ncol=2)

willingPayMoreBrandValues$frequencyPlot +
  labs(title='How Much More Are You Willing to Pay For a Brand Whose Values Align With Your Own?',
       x='Amount',y='Count') +
  theme(plot.title=element_text(hjust=.5))
myPurchaseSupportCauseMakesImpact$frequencyPlot +
  labs(title='I Feel My Purchases are Making an Impact When the Brand Supports a Cause I Believe In',
       y='Count') + theme(plot.title=element_text(hjust=.5))

productsUsed$overall +
  labs(title='Products Used in the Past 12 Months',
       y='Percent Young People Using') +
  scale_y_continuous(breaks=pretty_breaks(10))

tickKeepDelete <-
  whichAppDeleteGroup$Frequency$data %>%
  count(XTRwhich_delete_first) %>%
  mutate(
    p=n/sum(n),
    variable=gsub(' ','',XTRwhich_delete_first)
    ) %>%
  select(variable, Delete=p) %>%
  left_join(
    productsUsed$overall$data %>%
      select(variable, ticked=value) %>%
      mutate(variable=gsub('_','',variable))
  ) %>%
  left_join(
    whichAppKeepGroup$Frequency$data %>%
      count(XTRwhich_keep_most) %>%
      mutate(
        p=n/sum(n),
        variable=gsub(' ','',XTRwhich_keep_most)
      ) %>%
      select(variable, Keep=p)
  ) %>%
  filter(ticked>0)

tickKeepDelete.m <-
  melt(tickKeepDelete,
     id.vars=c('variable','ticked'),
     measure.vars = c('Delete','Keep'),
     variable.name = 'Group')

ggplot(tickKeepDelete.m, aes(x=reorder(variable, -ticked), y=ticked/2)) +
  geom_bar(stat='identity', alpha=.6, fill='#9e1f63') +
  geom_bar(aes(y=value, fill=Group), stat='identity', position='stack') +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.title=element_text(hjust = .5),
    legend.title=element_blank()) +
  scale_y_continuous(breaks=pretty_breaks(20)) +
  labs(title='App Usage and Preference', x='Application',y='Percent Ticked')

npAware <-
  bind_rows(
    tibble(
      Feature='We.Org',
      Value=weighted.mean(nonprofitAwareness.WeDotOrg$frequencyPlot$data$outcome,
                          nonprofitAwareness.WeDotOrg$frequencyPlot$data$count)
    ),
    tibble(
      Feature='DoSomething',
      Value=weighted.mean(nonprofitAwareness.DoSomething$frequencyPlot$data$outcome,
                          nonprofitAwareness.DoSomething$frequencyPlot$data$count)
    ),
    tibble(
      Feature='KeyClub',
      Value=weighted.mean(nonprofitAwareness.KeyClub$frequencyPlot$data$outcome,
                          nonprofitAwareness.KeyClub$frequencyPlot$data$count)
    ),
    tibble(
      Feature='4H',
      Value=weighted.mean(nonprofitAwareness.4H$frequencyPlot$data$outcome,
                          nonprofitAwareness.4H$frequencyPlot$data$count)
    ),
    tibble(
      Feature='Change.Org',
      Value=weighted.mean(nonprofitAwareness.ChangeDotOrg$frequencyPlot$data$outcome,
                          nonprofitAwareness.ChangeDotOrg$frequencyPlot$data$count)
    ),
    tibble(
      Feature='DonorsChoose',
      Value=weighted.mean(nonprofitAwareness.DonorsChoose$frequencyPlot$data$outcome,
                          nonprofitAwareness.DonorsChoose$frequencyPlot$data$count)
    )
  )
npAware.p <-
  ggplot(npAware, aes(x=reorder(Feature, -Value), y=Value)) +
  geom_bar(stat='identity', fill='#6ac6b4') +
  labs(x='Organization', y='How Aware',
       title='How Familiar Are You With These Organizations') +
  theme(plot.title = element_text(hjust=.5)) +
  scale_y_continuous(
    breaks=seq(-2,2,1),
    labels=c('Not Familiar','A little','Neutral','Somewhat','Very Familiar'),
    limits = c(-2,2)
    )

nonprofitAwareness.DoSomething$frequencyPlot +
  labs(title='How Familiar Are You With DoSomething.org', y='Count',x='') +
  theme(plot.title = element_text(hjust = .5))

grid.arrange(
  nonprofitAwareness.DoSomething$pivotPlot[[3]] + ggtitle('DoSomething') + ylim(c(-1.75,.1)) +
    theme(plot.title = element_text(hjust=.5), axis.text.x = element_text(angle=0))+
    scale_y_continuous(
      breaks=seq(-2,2,1),
      labels=c('Not Familiar','A little','Neutral','Somewhat','Very Familiar'),
      limits = c(-2,2)
    ),
  nonprofitAwareness.4H$pivotPlot[[7]] + ggtitle('4H') + ylim(c(-1.75,.1)) +
    theme(plot.title = element_text(hjust=.5), axis.text.x = element_text(angle=0))+
    scale_y_continuous(
      breaks=seq(-2,2,1),
      labels=c('Not Familiar','A little','Neutral','Somewhat','Very Familiar'),
      limits = c(-2,2)
    ),
  nonprofitAwareness.DonorsChoose$pivotPlot[[4]] + ggtitle('Donors Choose') + ylim(c(-1.75,.1)) +
    theme(plot.title = element_text(hjust=.5), axis.text.x = element_text(angle=0))+
    scale_y_continuous(
      breaks=seq(-2,2,1),
      labels=c('Not Familiar','A little','Neutral','Somewhat','Very Familiar'),
      limits = c(-2,2)
    ),
  nonprofitAwareness.ChangeDotOrg$pivotPlot[[6]] + ggtitle('Change.Org') + ylim(c(-1.75,.1)) +
    theme(plot.title = element_text(hjust=.5), axis.text.x = element_text(angle=0))+
    scale_y_continuous(
      breaks=seq(-2,2,1),
      labels=c('Not Familiar','A little','Neutral','Somewhat','Very Familiar'),
      limits = c(-2,2)
    ),
  nonprofitAwareness.KeyClub$pivotPlot[[4]] + ggtitle('Key Club') + ylim(c(-1.75,.1)) +
    theme(plot.title = element_text(hjust=.5), axis.text.x = element_text(angle=0))+
    scale_y_continuous(
      breaks=seq(-2,2,1),
      labels=c('Not Familiar','A little','Neutral','Somewhat','Very Familiar'),
      limits = c(-2,2)
    ),
  nonprofitAwareness.WeDotOrg$pivotPlot[[5]] + ggtitle('We.Org') + ylim(c(-1.75,.1)) +
    theme(plot.title = element_text(hjust=.5), axis.text.x = element_text(angle=0))+
    scale_y_continuous(
      breaks=seq(-2,2,1),
      labels=c('Not Familiar','A little','Neutral','Somewhat','Very Familiar'),
      limits = c(-2,2)
    ),
  ncol=3, top=textGrob("How Familiar Are You with These Organizations",gp=gpar(fontsize=20)),
  bottom='X-Axis is Age, Y-Axis is Familiarity'
  )

raceSum <-
  set %>%
  filter(!is.na(non_profit_awareness.DoSomething_org)) %>%
  mutate(
    non_profit_awareness.DoSomething_org =
      case_when(
        non_profit_awareness.DoSomething_org == 'Not at all familiar' ~ -2,
        non_profit_awareness.DoSomething_org == '2' ~ -1,
        non_profit_awareness.DoSomething_org == '3' ~ 0,
        non_profit_awareness.DoSomething_org == '4' ~ 1,
        non_profit_awareness.DoSomething_org == 'Very familiar' ~ 2
      )
  ) %>%
  group_by(race) %>%
  summarise(avgValue=mean(non_profit_awareness.DoSomething_org))

ggplot(raceSum, aes(x=race, y=avgValue)) +
  geom_bar(stat='identity', fill='#6ac6b4') +
  labs(x='Race',y='Famliarity',title='Familiarity by Race') +
  theme(plot.title = element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1)) +
  scale_y_continuous(
    breaks=seq(-2,2,1),
    labels=c('Not Familiar','A little','Neutral','Somewhat','Very Familiar'),
    limits = c(-2,2)
  )

ggplot(whereSeeDoSomething$overall$data, aes(y=value, x=reorder(variable, -value))) +
  geom_bar(stat='identity', alpha=.5, fill='#6ac6b4') +
  geom_text(aes(y=.0025,label=variable), angle=0, size=3, hjust=0) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust=.5)
  ) +
  labs(title='Where Did You See DoSomething', x='',y='Percent Ticked') +
  coord_flip()

ggplot(howEngageDoSomething$overall$data %>%
  filter(variable != 'None_of_the_above_I_have_not_engaged_with_DoSomething_org'),
  aes(y=value, x=reorder(variable, -value))) +
  geom_bar(stat='identity', alpha=.5, fill='#6ac6b4') +
  geom_text(aes(y=.0025,label=variable), angle=0, size=3, hjust=0) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust=.5)
  ) +
  labs(title='How Did You Engage with DoSomething', x='',y='Percent Ticked') +
  coord_flip()

npsBreakdown +
  labs(title='NPS Breakdown; Score = -20',x='NPS') +
  geom_text(stat='count',aes(label=..count..), vjust=-.7, size=3) +
  geom_text(data=tibble(x=9.5,y=145),aes(x=x,y=y, label=percent((69+112)/nrow(npsBreakdown$data)))) +
  geom_text(data=tibble(x=3,y=145),aes(x=x,y=y, label=percent((14+9+16+26+50+111+106)/nrow(npsBreakdown$data)))) +
  geom_text(data=tibble(x=7.5,y=145),aes(x=x,y=y, label=percent((132+124)/nrow(npsBreakdown$data))))

npsFamiliarGroupScores <-
  set %>%
  filter(non_profit_awareness.DoSomething_org=='Very familiar' & !is.na(nps)) %>%
  count(nps) %>%
  mutate(
    p=n/sum(n),
    group=ifelse(nps>=9,'promoter',ifelse(nps<7,'detract','neither'))) %>%
  group_by(group) %>%
  summarise(pcts=sum(p))
npsBreakdown.Familiar +
  labs(title='NPS Breakdown - Very Familiar w/ DoSomething; Score = 20',x='NPS') +
  geom_text(stat='count',aes(label=..count..), vjust=-.7, size=3) +
  geom_text(data=tibble(x=9.5,y=145),aes(x=x,y=y, label=percent(npsFamiliarGroupScores$pcts[3]))) +
  geom_text(data=tibble(x=3,y=145),aes(x=x,y=y, label=percent(npsFamiliarGroupScores$pcts[2]))) +
  geom_text(data=tibble(x=7.5,y=145),aes(x=x,y=y, label=percent(npsFamiliarGroupScores$pcts[1])))

# Willingness to take action  ----------------------------------------------
whichWhen <- apropos('^whichActWhen.')
for (j in 1:length(whichWhen)) {
  for (i in 1:length(get(whichWhen[j])$pivotPlot)) {
    print(paste(whichWhen[j],i,get(whichWhen[j])$pivotPlot[[i]]$labels$title))
  }
}
checkThis <- 'Group'
for (j in 1:length(whichWhen)) {
  for (i in 1:length(get(whichWhen[j])$pivotPlot)) {
    if (grepl(checkThis,paste(whichWhen[j],i,get(whichWhen[j])$pivotPlot[[i]]$labels$title))) {
      print(paste(whichWhen[j],i,get(whichWhen[j])$pivotPlot[[i]]$labels$title))
    }
  }
}

whenAct.ovr <-
  bind_rows(
    whichActWhen.beenPartOfCampaign$groupedPivotPlot[[9]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Took Part in a Campaign'),
    whichActWhen.boycottedBrandForIssue$groupedPivotPlot[[11]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Boycotted a Brand'),
    whichActWhen.collectItemsForHomeless$groupedPivotPlot[[8]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Collected Items for Homeless'),
    whichActWhen.contactedPolitician$groupedPivotPlot[[11]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Contacted Politician'),
    whichActWhen.donateTimeMoneyToCause$groupedPivotPlot[[11]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Donated Time/Money to Cause'),
    whichActWhen.leaderInClub$groupedPivotPlot[[9]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Held Leadership Role in Club'),
    whichActWhen.participateVolunteerOrg$groupedPivotPlot[[11]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Participated in Volunteer Org'),
    whichActWhen.plannedSocialImpactActivity$groupedPivotPlot[[10]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Planned an Activity for Social Impact'),
    whichActWhen.politicalEventsProtests$groupedPivotPlot[[7]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Attended Protest'),
    whichActWhen.purchasedBrandToSupportIssue$groupedPivotPlot[[10]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Purchased Brand to Support Cause'),
    whichActWhen.researchSocialIssueOutsideSchool$groupedPivotPlot[[7]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Researched Social Issue Outside School'),
    whichActWhen.signedOnlinePetition$groupedPivotPlot[[9]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Signed a Petition Online'),
    whichActWhen.startedDiscussionPolitics$groupedPivotPlot[[8]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Started Classroom Political Discussion'),
    whichActWhen.stoodUpToBullyForSomeone$groupedPivotPlot[[6]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Stood Up to a Bully'),
    whichActWhen.takenActionEncouragedOnline$groupedPivotPlot[[9]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Encouraged Others to Take Action Online'),
    tibble(
      avgVal=weighted.mean(
        whichActWhen.createdPetition$frequencyPlot$data$outcome,
        whichActWhen.createdPetition$frequencyPlot$data$count),
      Group='Created a Petition'
    ),
    tibble(
      avgVal=weighted.mean(
        whichActWhen.engagedCompanyToAdvocate$frequencyPlot$data$outcome,
        whichActWhen.engagedCompanyToAdvocate$frequencyPlot$data$count),
      Group='Engaged Company to Advocate for Cause'
    ),
    tibble(
      avgVal=weighted.mean(
        whichActWhen.startedCampaignToSolveProblem$frequencyPlot$data$outcome,
        whichActWhen.startedCampaignToSolveProblem$frequencyPlot$data$count),
      Group='Started a Campaign'
    )
  )

ggplot(whenAct.ovr, aes(x=reorder(Group,-avgVal), y=avgVal)) +
  geom_bar(stat='identity', fill='#6ac6b4') +
  labs(title='Which of the Following Actions Have You Taken & When? (Gen Pop)',
       x='',y='Average Response') +
  scale_y_continuous(
    labels=c('-1'='Unlikely To do','0'='Might Do','1'='Did In Past','2'='Done Past Year'),
    breaks=c(-1,0,1,2), limits=c(-1,2)
  ) +
  theme(
    plot.title=element_text(hjust=.5),
    axis.text.x = element_text(angle=30, hjust=1)
  )

# politics
whenActionsPolitics <-
  bind_rows(
    politicalEventsProtests$groupedPivotPlot[[3]]$data %>% mutate(quest='Attended Protest'),
    takenActionEncouragedOnline$groupedPivotPlot[[5]]$data %>% mutate(quest='Encouraged Others to Take Action Online'),
    contactedPolitician$groupedPivotPlot[[5]]$data %>% mutate(quest='Contacted Politician'),
    signedOnlinePetition$groupedPivotPlot[[5]]$data %>% mutate(quest='Signed a Petition Online'),
    participateVolunteerOrg$groupedPivotPlot[[5]]$data %>% mutate(quest='Participated in Volunteer Org'),
    startedDiscussionPolitics$groupedPivotPlot[[5]]$data %>% mutate(quest='Started Classroom Political Discussion'),
    purchasedBrandToSupportIssue$groupedPivotPlot[[4]]$data %>% mutate(quest='Purchased Brand to Support Cause'),
    boycottedBrandForIssue$groupedPivotPlot[[3]]$data %>% mutate(quest='Boycotted a Brand'),
    # stoodUpToBullyForSomeone
    leaderInClub$groupedPivotPlot[[5]]$data %>% mutate(quest='Held Leadership Role in Club'),
    collectItemsForHomeless$groupedPivotPlot[[4]]$data %>% mutate(quest='Collected Items for Homeless'),
    researchSocialIssueOutsideSchool$groupedPivotPlot[[4]]$data %>% mutate(quest='Researched Social Issue Outside School'),
    engagedCompanyToAdvocate$groupedPivotPlot[[3]]$data %>% mutate(quest='Engaged Company to Advocate for Cause'),
    plannedSocialImpactActivity$groupedPivotPlot[[5]]$data %>% mutate(quest='Planned an Activity for Social Impact'),
    donateTimeMoneyToCause$groupedPivotPlot[[5]]$data %>% mutate(quest='Donated Time/Money to Cause'),
    createdPetition$groupedPivotPlot[[2]]$data %>% mutate(quest='Created a Petition'),
    startedCampaignToSolveProblem$groupedPivotPlot[[3]]$data %>% mutate(quest='Started a Campaign'),
    beenPartOfCampaign$groupedPivotPlot[[5]]$data %>% mutate(quest='Took Part in a Campaign')
  ) %>%
  group_by(quest) %>%
  mutate(meanVal = mean(avgVal))

ggplot(whenActionsPolitics, aes(x=avgVal, y=reorder(quest, -meanVal), color=political_view)) +
  geom_point() +
  facet_wrap(~Group) +
  scale_x_continuous(
    labels=c('-1'='Unlikely To do','0'='Might Do','1'='Did Long Ago','2'='Done Past Year'),
    breaks=c(-1,0,1,2), limits=c(-1,2)
    ) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 25)) +
  labs(y='',x='',title='Which of the Following Actions Have You Taken & When?') +
  scale_colour_brewer(palette = 'RdYlBu',name='Political Views') +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))

ggplot(whenActionsPolitics %>% filter(Group=='Gen Pop'),
       aes(x=avgVal, y=reorder(quest, -meanVal), color=political_view)) +
  geom_point() +
  scale_x_continuous(
    labels=c('-1'='Unlikely To do','0'='Might Do','1'='Did Long Ago','2'='Done Past Year'),
    breaks=c(-1,0,1,2), limits=c(-1,2)
  ) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 25)) +
  labs(y='',x='',title='Which of the Following Actions Have You Taken & When?') +
  scale_colour_brewer(palette = 'RdYlBu',name='Political Views') +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))


whenActionsGender <-
  bind_rows(
    takenActionEncouragedOnline$groupedPivotPlot[[9]]$data %>% mutate(quest='Encouraged Others to Take Action Online'),
    contactedPolitician$groupedPivotPlot[[10]]$data %>% mutate(quest='Contacted Politician'),
    signedOnlinePetition$groupedPivotPlot[[10]]$data %>% mutate(quest='Signed a Petition Online'),
    participateVolunteerOrg$groupedPivotPlot[[12]]$data %>% mutate(quest='Participated in Volunteer Org'),
    purchasedBrandToSupportIssue$groupedPivotPlot[[9]]$data %>% mutate(quest='Purchased Brand to Support Cause'),
    boycottedBrandForIssue$groupedPivotPlot[[6]]$data %>% mutate(quest='Boycotted a Brand'),
    collectItemsForHomeless$groupedPivotPlot[[6]]$data %>% mutate(quest='Collected Items for Homeless'),
    researchSocialIssueOutsideSchool$groupedPivotPlot[[9]]$data %>% mutate(quest='Researched Social Issue Outside School'),
    engagedCompanyToAdvocate$groupedPivotPlot[[7]]$data %>% mutate(quest='Engaged Company to Advocate for Cause'),
    plannedSocialImpactActivity$groupedPivotPlot[[10]]$data %>% mutate(quest='Planned an Activity for Social Impact'),
    donateTimeMoneyToCause$groupedPivotPlot[[11]]$data %>% mutate(quest='Donated Time/Money to Cause'),
    createdPetition$groupedPivotPlot[[5]]$data %>% mutate(quest='Created a Petition'),
    startedCampaignToSolveProblem$groupedPivotPlot[[7]]$data %>% mutate(quest='Started a Campaign'),
    beenPartOfCampaign$groupedPivotPlot[[9]]$data %>% mutate(quest='Took Part in a Campaign')
  ) %>%
  group_by(quest) %>%
  mutate(meanVal = mean(avgVal)) %>%
  filter(sex %in% c('Male','Female','Non-binary'))

ggplot(whenActionsGender, aes(x=avgVal, y=reorder(quest, -meanVal), color=sex)) +
  geom_point() +
  facet_wrap(~Group) +
  scale_x_continuous(
    labels=c('-1'='Unlikely To do','0'='Might Do','1'='Did Long Ago','2'='Done Past Year'),
    breaks=c(-1,0,1,2), limits=c(-1,2)
  ) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 25)) +
  labs(y='',x='',title='') +
  scale_colour_brewer(palette = 'Set2',name='Gender') +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))

ggplot(whenActionsGender %>% filter(Group=='Gen Pop'),
       aes(x=avgVal, y=reorder(quest, -meanVal), color=sex)) +
  geom_point() +
  scale_x_continuous(
    labels=c('-1'='Unlikely To do','0'='Might Do','1'='Did Long Ago','2'='Done Past Year'),
    breaks=c(-1,0,1,2), limits=c(-1,2)
  ) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 25)) +
  labs(y='',x='',title='') +
  scale_colour_brewer(palette = 'Set2',name='Gender') +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))

whenActionsRace <-
  bind_rows(
    politicalEventsProtests$groupedPivotPlot[[7]]$data %>% mutate(quest='Attended Protest'),
    takenActionEncouragedOnline$groupedPivotPlot[[7]]$data %>% mutate(quest='Encouraged Others to Take Action Online'),
    contactedPolitician$groupedPivotPlot[[8]]$data %>% mutate(quest='Contacted Politician'),
    signedOnlinePetition$groupedPivotPlot[[8]]$data %>% mutate(quest='Signed a Petition Online'),
    participateVolunteerOrg$groupedPivotPlot[[9]]$data %>% mutate(quest='Participated in Volunteer Org'),
    startedDiscussionPolitics$groupedPivotPlot[[8]]$data %>% mutate(quest='Started Classroom Political Discussion'),
    purchasedBrandToSupportIssue$groupedPivotPlot[[7]]$data %>% mutate(quest='Purchased Brand to Support Cause'),
    stoodUpToBullyForSomeone$groupedPivotPlot[[4]]$data %>% mutate(quest='Stood Up to a Bully'),
    engagedCompanyToAdvocate$groupedPivotPlot[[5]]$data %>% mutate(quest='Engaged Company to Advocate for Cause'),
    plannedSocialImpactActivity$groupedPivotPlot[[8]]$data %>% mutate(quest='Planned an Activity for Social Impact'),
    donateTimeMoneyToCause$groupedPivotPlot[[8]]$data %>% mutate(quest='Donated Time/Money to Cause')
  ) %>%
  group_by(quest) %>%
  mutate(meanVal = mean(avgVal)) %>%
  filter(race %in% c('Asian','Black','Hispanic/Latino','White','Multiracial'))

ggplot(whenActionsRace, aes(x=avgVal, y=reorder(quest, -meanVal), color=race)) +
  geom_point() +
  facet_wrap(~Group) +
  scale_x_continuous(
    labels=c('-1'='Unlikely To do','0'='Might Do','1'='Did Long Ago','2'='Done Past Year'),
    breaks=c(-1,0,1,2), limits=c(-1,2)
  ) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 25)) +
  labs(y='',x='',title='') +
  scale_colour_brewer(palette = 'Set2',name='Race') +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))

for (i in 1:length(beenPartOfCampaign$pivotPlot)) {
  print(paste(i,beenPartOfCampaign$pivotPlot[[i]]$labels$title))
}


whenActionsReligion <-
  bind_rows(
    takenActionEncouragedOnline$groupedPivotPlot[[4]]$data %>% mutate(quest='Encouraged Others to Take Action Online'),
    contactedPolitician$groupedPivotPlot[[4]]$data %>% mutate(quest='Contacted Politician'),
    signedOnlinePetition$groupedPivotPlot[[4]]$data %>% mutate(quest='Signed a Petition Online'),
    participateVolunteerOrg$groupedPivotPlot[[4]]$data %>% mutate(quest='Participated in Volunteer Org'),
    startedDiscussionPolitics$groupedPivotPlot[[4]]$data %>% mutate(quest='Started Classroom Political Discussion'),
    leaderInClub$groupedPivotPlot[[4]]$data %>% mutate(quest='Held Leadership Role in Club'),
    collectItemsForHomeless$groupedPivotPlot[[3]]$data %>% mutate(quest='Collected Items for Homeless'),
    engagedCompanyToAdvocate$groupedPivotPlot[[2]]$data %>% mutate(quest='Engaged Company to Advocate for Cause'),
    plannedSocialImpactActivity$groupedPivotPlot[[4]]$data %>% mutate(quest='Planned an Activity for Social Impact'),
    donateTimeMoneyToCause$groupedPivotPlot[[4]]$data %>% mutate(quest='Donated Time/Money to Cause'),
    startedCampaignToSolveProblem$groupedPivotPlot[[2]]$data %>% mutate(quest='Started a Campaign'),
    beenPartOfCampaign$groupedPivotPlot[[4]]$data %>% mutate(quest='Took Part in a Campaign')
  ) %>%
  group_by(quest) %>%
  mutate(meanVal = mean(avgVal))

ggplot(whenActionsReligion, aes(x=avgVal, y=reorder(quest, -meanVal), color=attend_religious_services_freq)) +
  geom_point() +
  facet_wrap(~Group, ncol=2) +
  scale_x_continuous(
    labels=c('-1'='Unlikely To do','0'='Might Do','1'='Did Long Ago','2'='Done Past Year'),
    breaks=c(-1,0,1,2), limits=c(-1,2)
  ) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 25)) +
  labs(y='',x='',title='') +
  scale_colour_brewer(palette = 'RdBu',name='Religiosity') +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))

for (i in 1:length(beenPartOfCampaign$pivotPlot)) {
  print(paste(i,beenPartOfCampaign$pivotPlot[[i]]$labels$title))
}

p1 <-
  signedOnlinePetition$pivotPlot[[9]] +
  geom_bar(aes(x=age,y=avgVal,fill='#6ac6b4'),stat='identity',alpha=.8) +
  geom_smooth(
    aes(x=age, y=avgVal),
    method='lm', se=F, color='#9e1f63', linetype='dashed', size=.75
  ) +
  scale_y_continuous(breaks=c(-1,0,1,2), limits=c(-1,2)) +
  theme(plot.title = element_text(hjust=.5),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle=0, hjust=.5)) +
  labs(
    x='Age',y='',
    title='Signing an Online Petition')

p2 <-
  whichActWhen.plannedSocialImpactActivity$pivotPlot[[8]] +
  geom_bar(aes(x=age,y=avgVal,fill='#6ac6b4'),stat='identity',alpha=.8) +
  geom_smooth(
    aes(x=age, y=avgVal),
    method='lm', se=F, color='#9e1f63', linetype='dashed', size=.75
  ) +
  scale_y_continuous(breaks=c(-1,0,1,2), limits=c(-1,2)) +
  theme(plot.title = element_text(hjust=.5),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle=0, hjust=.5)) +
  labs(
    x='Age',
    title='Planned Social impact Activity'
    )

grid.arrange(
  p1,p2,ncol=2,
  top=textGrob("Comparison of Action Taken by Age",gp=gpar(fontsize=20)),
  bottom="Values range from -2 to 2 with -2 representing 'Would Never Do' and 2 representing 'Done Recently'"
  )

grid.arrange(
  ggplot(issuesTakenAction.genpop$overall$data, aes(x=reorder(variable,-value),y=value)) +
    geom_bar(stat='identity', alpha=.8, fill='#6ac6b4') +
    geom_text(aes(label=percent(value)),vjust=-.5,size=2.5) +
    labs(
      title='Gen Pop',
      x=paste0('Average # Ticked = ',round(sum(issuesTakenAction.genpop$overall$data$value),2)),
      y='Percent Ticked'
    ) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          plot.title = element_text(hjust = .5)) +
    scale_y_continuous(breaks=pretty_breaks(10),limits=c(0,.4)),
  ggplot(issuesTakenAction.members$overall$data, aes(x=reorder(variable,-value),y=value)) +
    geom_bar(stat='identity', alpha=.8, fill='#6ac6b4') +
    geom_text(aes(label=percent(value)),vjust=-.5,size=2.8) +
    labs(
      title='Members',
      x=paste0('Average # Ticked = ',round(sum(issuesTakenAction.members$overall$data$value),2)),
      y='Percent Ticked'
    ) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          plot.title = element_text(hjust = .5)) +
    scale_y_continuous(breaks=pretty_breaks(10),limits=c(0,.4)),
  ncol=2, top='Which Have You Taken Action on in the Past 12 months?'
)

levs <-
  c('Very conservative: Avg # Ticked = 2.7', 'Conservative: Avg # Ticked = 2.79',
    'Moderate: Avg # Ticked = 3.02','Liberal: Avg # Ticked = 4.6',
    'Very Liberal: Avg # Ticked = 4.99')
ggplot(issuesTakenAction.genpop$pivotPlots$political_view$data,
       aes(x=reorder(variable, -value), y=value)) +
  geom_bar(stat='identity', alpha=.8, fill='#6ac6b4') +
  geom_text(aes(label=percent(value)),hjust=-.1,size=2) +
  facet_wrap(~factor(fac_labs,levels = levs), labeller = label_value, ncol=3) +
  labs(
    x='',
    title='Gen Pop',y='Percentage Ticked'
  ) +
  theme(plot.title = element_text(hjust = .5)) +
  scale_y_continuous(breaks=pretty_breaks(6),limits=c(0,.58)) +
  coord_flip()

ggplot(issuesTakenAction.genpop$pivotPlots$political_view$data %>%
         filter(grepl('Very',political_view)),
       aes(x=reorder(variable, -value), y=value, fill=political_view)) +
  geom_bar(aes(width=.75),stat='identity', alpha=.8, position='dodge') +
  labs(
    x='',
    title='Gen Pop',y='Percentage Ticked'
  ) +
  scale_fill_manual(name = 'Political View', values=c('red1','royalblue4')) +
  theme(plot.title = element_text(hjust = .5)) +
  scale_y_continuous(breaks=pretty_breaks(6),limits=c(0,.58)) +
  coord_flip()

levs <-
  c('Very conservative: Avg # Ticked = 1.93', 'Conservative: Avg # Ticked = 2.61',
    'Moderate: Avg # Ticked = 3.54','Liberal: Avg # Ticked = 4.62',
    'Very Liberal: Avg # Ticked = 5.96')
ggplot(issuesTakenAction.members$pivotPlots$political_view$data, aes(x=reorder(variable, -value), y=value)) +
  geom_bar(stat='identity', alpha=.8, fill='#6ac6b4') +
  geom_text(aes(label=percent(value)),hjust=-.1,size=2) +
  facet_wrap(~factor(fac_labs,levels = levs), labeller = label_value, ncol=3) +
  labs(
    x='',
    title='Members',y='Percentage Ticked'
  ) +
  theme(plot.title = element_text(hjust = .5)) +
  scale_y_continuous(breaks=pretty_breaks(6),limits=c(0,.62)) +
  coord_flip()

ggplot(issuesTakenAction.genpop$pivotPlots$sex$data %>%
         filter(sex %in% c('Male','Female','Non-binary')),
       aes(x=reorder(variable, -value), y=value)) +
  geom_bar(stat='identity', alpha=.8, fill='#6ac6b4') +
  geom_text(aes(label=percent(value)),hjust=-.1,size=2.6) +
  facet_wrap(~fac_labs, labeller = label_value, ncol=3) +
  labs(
    x='',
    title='Gen Pop',y='Percentage Ticked'
  ) +
  theme(plot.title = element_text(hjust = .5)) +
  scale_y_continuous(breaks=pretty_breaks(6),limits = c(0,.92)) +
  coord_flip()

ggplot(issuesTakenAction.members$pivotPlots$sex$data %>%
         filter(sex %in% c('Male','Female','Non-binary')),
       aes(x=reorder(variable, -value), y=value)) +
  geom_bar(stat='identity', alpha=.8, fill='#6ac6b4') +
  geom_text(aes(label=percent(value)),hjust=-.1,size=2.6) +
  facet_wrap(~fac_labs, labeller = label_value, ncol=3) +
  labs(
    x='',
    title='Members',y='Percentage Ticked'
  ) +
  theme(plot.title = element_text(hjust = .5)) +
  scale_y_continuous(breaks=pretty_breaks(6),limits = c(0,.92)) +
  coord_flip()

ggplot(issuesTakenAction$pivotPlots$race$data %>%
         filter(race %in% c('Asian','Black','White','Hispanic/Latino','Multiracial')),
       aes(x=reorder(variable, -value), y=value)) +
  geom_bar(stat='identity', alpha=.8, fill='#6ac6b4') +
  geom_text(aes(label=percent(value)),hjust=-.1,size=2.3) +
  facet_wrap(~fac_labs, labeller = label_value, ncol=3) +
  labs(
    x='',
    title='Gen Pop',y='Percentage Ticked'
  ) +
  theme(plot.title = element_text(hjust = .5)) +
  scale_y_continuous(breaks=pretty_breaks(6),limits = c(0,.5)) +
  coord_flip()



ggplot(issuesTakenAction.members$pivotPlots$attend_religious_services_freq$data %>%
         filter(attend_religious_services_freq %in% c('Multiple times a week','Never, agnostic or atheist')),
       aes(x=reorder(variable, -value), y=value)) +
  geom_bar(stat='identity', alpha=.8, fill='#6ac6b4') +
  geom_text(aes(label=percent(value)),hjust=-.1,size=2.3) +
  facet_wrap(~fac_labs, labeller = label_value, ncol=2) +
  labs(
    x='',
    title='How Often do You Attend Religious Services ',y='Percentage Ticked'
  ) +
  theme(plot.title = element_text(hjust = .5)) +
  scale_y_continuous(breaks=pretty_breaks(6),limits = c(0,.5)) +
  coord_flip()

ggplot(volunteerReason$overall$data, aes(x=reorder(variable, -value), y=value)) +
  geom_bar(stat='identity', alpha=.8, fill='#6ac6b4') +
  geom_text(aes(label=percent(value)),hjust=-.1,size=2.8) +
  labs(x='',title='Why Did You Volunteer?',y='Percentage Ticked') +
  theme(plot.title = element_text(hjust = .5)) +
  scale_y_continuous(breaks=pretty_breaks(6),limits = c(0,.75)) +
  coord_flip()

planVote <-
  set %>%
  filter(plan_to_vote_if_eligible%in%c('Yes','No','Undecided')) %>%
  group_by(Group,plan_to_vote_if_eligible) %>%
  summarise(count=sum(weight))

ggplot(planVote, aes(x=plan_to_vote_if_eligible,y=count)) +
  geom_bar(stat='identity',fill='#6ac6b4',width = .8) +
  geom_text(aes(x=plan_to_vote_if_eligible,y=count,label=round(count)),vjust=-.1,size=3.4) +
  facet_wrap(~Group) +
  labs(title='Do You Plan to Vote', x='',y='') +
  theme(plot.title=element_text(hjust=.5))

planvoteElig <-
  set %>%
  filter(!is.na(vote_when_eligible)) %>%
  group_by(Group,vote_when_eligible) %>%
  summarise(count=sum(weight))
ggplot(planvoteElig, aes(x=vote_when_eligible,y=count)) +
  geom_bar(stat='identity',fill='#6ac6b4',width = .8) +
  geom_text(aes(x=vote_when_eligible,y=count,label=round(count)),vjust=-.1,size=3.4) +
  facet_wrap(~Group) +
  labs(title='Do You Plan to Vote When You Are Eligible', x='',y='') +
  theme(plot.title=element_text(hjust=.5))

# The Future --------------------------------------------------------------

for (i in 1:length(willOccur.iRunForPublicOffice$pivotPlot)) {
  print(paste(i,willOccur.iRunForPublicOffice$pivotPlot[[i]]$labels$title))
}

for (i in 1:length(willOccur.iWillGetRich$pivotPlot)) {
  print(paste(i,willOccur.iWillGetRich$pivotPlot[[i]]$labels$title))
}

for (i in 1:length(willOccur.iWillChangeTheWorld$pivotPlot)) {
  print(paste(i,willOccur.iWillChangeTheWorld$pivotPlot[[i]]$labels$title))
}

for (i in 1:length(willOccur.iWillLiveLong$pivotPlot)) {
  print(paste(i,willOccur.iWillLiveLong$pivotPlot[[i]]$labels$title))
}

for (i in 1:length(willOccur.iWillHaveBetterLifeThanParents$pivotPlot)) {
  print(paste(i,willOccur.iWillHaveBetterLifeThanParents$pivotPlot[[i]]$labels$title))
}

#DS Membership
willOccur.iRunForPublicOffice$groupedPivotPlot[[1]]
willOccur.iWillChangeTheWorld$groupedPivotPlot[[5]]
willOccur.iWillLiveLong$groupedPivotPlot[[7]]

whichWillOccur <-
  bind_rows(
    willOccur.iRunForPublicOffice$groupedPivotPlot[[1]]$data %>%
      filter(reportbacks=='Gen Pop') %>%
      mutate(quest='I Will Run for Public Office'),
    tibble(quest='I Will Get Rich',
           avgVal=
             weighted.mean(willOccur.iWillGetRich$frequencyPlot$data$outcome,
                           willOccur.iWillGetRich$frequencyPlot$data$count)),
    willOccur.iWillChangeTheWorld$groupedPivotPlot[[5]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(quest='I Will Change the World'),
    willOccur.iWillLiveLong$pivotPlot[[7]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(quest='I Will Live a Long Life'),
    tibble(quest='I Will Have a Better Life than My Parents',
           avgVal=
             weighted.mean(willOccur.iWillHaveBetterLifeThanParents$frequencyPlot$data$outcome,
                           willOccur.iWillHaveBetterLifeThanParents$frequencyPlot$data$count))
  ) %>% ungroup() %>%
  select(quest, avgVal)

ggplot(whichWillOccur, aes(x=reorder(quest,-avgVal), y=avgVal)) +
  geom_bar(stat='identity', fill='#6ac6b4') +
  geom_text(aes(x=quest,y=avgVal,label=percent(avgVal)),hjust=-.11,size=3.4) +
  labs(title='Prospects for the Future (Gen Pop)', x='',y='Percent Yes') +
  scale_y_continuous(breaks=pretty_breaks(10),limits = c(0,.55)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  theme(
    plot.title=element_text(hjust=.5)
    ) +
  coord_flip()

#Gender
willOccur.Gender <-
  bind_rows(
    willOccur.iRunForPublicOffice$groupedPivotPlot[[7]]$data %>% mutate(quest='I Will Run for Public Office'),
    willOccur.iWillGetRich$groupedPivotPlot[[4]]$data %>% mutate(quest='I Will Get Rich'),
    willOccur.iWillChangeTheWorld$groupedPivotPlot[[4]]$data %>% mutate(quest='I Will Change the World'),
    willOccur.iWillLiveLong$groupedPivotPlot[[6]]$data %>% mutate(quest='I Will Live a Long Life')
  ) %>%
  filter(sex %in% c('Male','Female','Non-binary')) %>%
  group_by(quest) %>%
  mutate(meanVal=mean(avgVal))

ggplot(willOccur.Gender, aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=sex), size=2) +
  facet_wrap(~Group) +
  labs(title='Which of the Following Will Occur?',x='Percent Yes',y='') +
  scale_colour_brewer(palette = 'Set2') +
  scale_x_continuous(breaks=pretty_breaks(10)) +
  guides(colour=guide_legend(title="Gender")) +
  theme(plot.title=element_text(hjust=.5))

#Politics
willOccur.iRunForPublicOffice$groupedPivotPlot[[4]]
willOccur.iWillChangeTheWorld$groupedPivotPlot[[3]]
willOccur.iWillLiveLong$groupedPivotPlot[[4]]

willOccur.Politics <-
  bind_rows(
    willOccur.iRunForPublicOffice$groupedPivotPlot[[4]]$data %>% mutate(quest='I Will Run for Public Office'),
    willOccur.iWillChangeTheWorld$groupedPivotPlot[[3]]$data %>% mutate(quest='I Will Change the World'),
    willOccur.iWillLiveLong$groupedPivotPlot[[4]]$data %>% mutate(quest='I Will Live a Long Life')
  ) %>%
  group_by(quest) %>%
  mutate(meanVal=mean(avgVal))

ggplot(
  willOccur.iRunForPublicOffice$groupedPivotPlot[[4]]$data,
  aes(x=political_view, y=avgVal, fill=Group)
  ) +
  geom_bar(stat='identity', position='dodge',alpha=.8) +
  geom_smooth(method='lm', aes(group=Group, colour=Group), se=F, linetype='dashed') +
  labs(title='I Will Run For Public Office', y='Percent Yes',x='') +
  scale_color_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  theme(plot.title = element_text(hjust=.5)) +
  scale_y_continuous(breaks=pretty_breaks(8))


ggplot(willOccur.Politics, aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=political_view), size=2) +
  facet_wrap(~Group) +
  labs(title='Which of the Following Will Occur?',x='Percent Yes',y='') +
  scale_colour_brewer(palette = 'RdYlBu') +
  scale_x_continuous(breaks=pretty_breaks(10)) +
  guides(colour=guide_legend(title="Political Views")) +
  theme(plot.title=element_text(hjust=.5))

# Top Issues --------------------------------------------------------------

issuesOvr <-
  bind_rows(
    topIssues.Terrorism$groupedPivotPlot[[9]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Terrorism'),
    tibble(
      avgVal=weighted.mean(
        topIssues.Education$frequencyPlot$data$outcome,
        topIssues.Education$frequencyPlot$data$count),
      Group='Education'
    ),
    tibble(
      avgVal=weighted.mean(
        topIssues.RacialJustice$frequencyPlot$data$outcome,
        topIssues.RacialJustice$frequencyPlot$data$count),
      Group='Racial Justice'
    ),
    tibble(
      avgVal=weighted.mean(
        topIssues.Immigration$frequencyPlot$data$outcome,
        topIssues.Immigration$frequencyPlot$data$count),
      Group='Immigration'
    ),
    tibble(
      avgVal=weighted.mean(
        topIssues.ClimateChange$frequencyPlot$data$outcome,
        topIssues.ClimateChange$frequencyPlot$data$count),
      Group='Climate Change'
    ),
    tibble(
      avgVal=weighted.mean(
        topIssues.GenderEquality$frequencyPlot$data$outcome,
        topIssues.GenderEquality$frequencyPlot$data$count),
      Group='Gender Equality'
    ),
    tibble(
      avgVal=weighted.mean(
        topIssues.Crime$frequencyPlot$data$outcome,
        topIssues.Crime$frequencyPlot$data$count),
      Group='Crime'
    ),
    tibble(
      avgVal=weighted.mean(
        topIssues.Poverty$frequencyPlot$data$outcome,
        topIssues.Poverty$frequencyPlot$data$count),
      Group='Poverty'
    ),
    tibble(
      avgVal=weighted.mean(
        topIssues.IncomeInequality$frequencyPlot$data$outcome,
        topIssues.IncomeInequality$frequencyPlot$data$count),
      Group='Income Inequality'
    ),
    topIssues.Jobs$groupedPivotPlot[[8]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Jobs'),
    topIssues.Military$groupedPivotPlot[[8]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Military'),
    topIssues.CountryDebt$groupedPivotPlot[[11]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Federal Debt'),
    tibble(
      avgVal=weighted.mean(
        topIssues.AccessHealthcare$frequencyPlot$data$outcome,
        topIssues.AccessHealthcare$frequencyPlot$data$count),
      Group='Access to Healthcare'
    ),
    tibble(
      avgVal=weighted.mean(
        topIssues.CostsHealthcare$frequencyPlot$data$outcome,
        topIssues.CostsHealthcare$frequencyPlot$data$count),
      Group='Costs of Healthcare'
    ),
    topIssues.Economy$groupedPivotPlot[[7]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Economy'),
    tibble(
      avgVal=weighted.mean(
        topIssues.CollegeAffordability$frequencyPlot$data$outcome,
        topIssues.CollegeAffordability$frequencyPlot$data$count),
      Group='College Affordability'
    ),
    topIssues.TaxPolicy$groupedPivotPlot[[8]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Tax Poliicy'),
    tibble(
      avgVal=weighted.mean(
        topIssues.CriminalJusticeReform$frequencyPlot$data$outcome,
        topIssues.CriminalJusticeReform$frequencyPlot$data$count),
      Group='Criminal Justice Reform'
    ),
    topIssues.GunPolicy$groupedPivotPlot[[9]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Gun Policy'),
    tibble(
      avgVal=weighted.mean(
        topIssues.Environment$frequencyPlot$data$outcome,
        topIssues.Environment$frequencyPlot$data$count),
      Group='The Environment'
    ),
    tibble(
      avgVal=weighted.mean(
        topIssues.ReligiousIntolerance$frequencyPlot$data$outcome,
        topIssues.ReligiousIntolerance$frequencyPlot$data$count),
      Group='Religious Intolerance'
    ),
    topIssues.DrugOverdoses$groupedPivotPlot[[9]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Drug Overdoses')
  )

for (i in 1:length(topIssues.DrugOverdoses$pivotPlot)) {
  print(paste(i,topIssues.DrugOverdoses$pivotPlot[[i]]$labels$title))
}

ggplot(issuesOvr, aes(x=reorder(Group,-avgVal), y=avgVal)) +
  geom_bar(stat='identity', fill='#6ac6b4') +
  geom_text(aes(x=Group,y=avgVal,label=round(avgVal,2)),hjust=-.11,size=3.4) +
  labs(title='Which of these Issues are Most Important, in Order? (Gen Pop)',
       x='',y='Average Importance Rank') +
  scale_y_continuous(breaks=seq(0,6,1),limits = c(0,6),labels = c('','1st','2nd','3rd','4th','5th','Not Top 5')) +
  theme(
    plot.title=element_text(hjust=.5)
  ) +
  coord_flip()

issues <- apropos('^topIssues')
for (j in 1:length(issues)) {
  for (i in 1:length(get(issues[j])$pivotPlot)) {
    if (grepl('sex',paste(issues[j],i,get(issues[j])$pivotPlot[[i]]$labels$title))) {
      print(paste(issues[j],i,get(issues[j])$pivotPlot[[i]]$labels$title))
    }
  }
}

c('political_view','sex')

# political view
topIssues.AccessHealthcare$groupedPivotPlot[[4]]$data %>% mutate()
topIssues.ClimateChange$groupedPivotPlot[[3]]$data %>% mutate()
topIssues.CollegeAffordability$groupedPivotPlot[[3]]$data %>% mutate()
topIssues.CountryDebt$groupedPivotPlot[[5]]$data %>% mutate()
topIssues.Crime$groupedPivotPlot[[4]]$data %>% mutate()
topIssues.CriminalJusticeReform$groupedPivotPlot[[3]]$data %>% mutate()
topIssues.DrugOverdoses$groupedPivotPlot[[4]]$data %>% mutate()
topIssues.Economy$groupedPivotPlot[[4]]$data %>% mutate()
topIssues.Education$groupedPivotPlot[[2]]$data %>% mutate()
topIssues.Environment$groupedPivotPlot[[2]]$data %>% mutate()
topIssues.GenderEquality$groupedPivotPlot[[3]]$data %>% mutate()
topIssues.GunPolicy$groupedPivotPlot[[3]]$data %>% mutate()
topIssues.Immigration$groupedPivotPlot[[3]]$data %>% mutate()
topIssues.IncomeInequality$groupedPivotPlot[[2]]$data %>% mutate()
topIssues.Jobs$groupedPivotPlot[[4]]$data %>% mutate()
topIssues.Military$groupedPivotPlot[[5]]$data %>% mutate()
topIssues.Poverty$groupedPivotPlot[[3]]$data %>% mutate()
topIssues.RacialJustice$groupedPivotPlot[[1]]$data %>% mutate()
# topIssues.ReligiousIntolerance$groupedPivotPlot[[2]]
topIssues.TaxPolicy$groupedPivotPlot[[4]]$data %>% mutate()
topIssues.Terrorism$groupedPivotPlot[[4]]$data %>% mutate()

topIssues.Politics <-
  bind_rows(
    topIssues.AccessHealthcare$groupedPivotPlot[[4]]$data %>% mutate(quest='Access to Healthcare'),
    topIssues.ClimateChange$groupedPivotPlot[[3]]$data %>% mutate(quest='Climate Change'),
    topIssues.CollegeAffordability$groupedPivotPlot[[3]]$data %>% mutate(quest='College Affordability'),
    topIssues.CountryDebt$groupedPivotPlot[[5]]$data %>% mutate(quest='Federal Debt'),
    topIssues.Crime$groupedPivotPlot[[4]]$data %>% mutate(quest='Crime'),
    topIssues.CriminalJusticeReform$groupedPivotPlot[[3]]$data %>% mutate(quest='Criminal Justice Reform'),
    topIssues.DrugOverdoses$groupedPivotPlot[[4]]$data %>% mutate(quest='Drug Overdose'),
    topIssues.Economy$groupedPivotPlot[[4]]$data %>% mutate(quest='Economy'),
    topIssues.Education$groupedPivotPlot[[2]]$data %>% mutate(quest='Education'),
    topIssues.Environment$groupedPivotPlot[[2]]$data %>% mutate(quest='The Environment'),
    topIssues.GenderEquality$groupedPivotPlot[[3]]$data %>% mutate(quest='Gender Equality'),
    topIssues.GunPolicy$groupedPivotPlot[[3]]$data %>% mutate(quest='Gun Policy'),
    topIssues.Immigration$groupedPivotPlot[[3]]$data %>% mutate(quest='Immigration'),
    topIssues.IncomeInequality$groupedPivotPlot[[2]]$data %>% mutate(quest='Income Inequality'),
    topIssues.Jobs$groupedPivotPlot[[4]]$data %>% mutate(quest='Jobs'),
    topIssues.Military$groupedPivotPlot[[5]]$data %>% mutate(quest='Military'),
    topIssues.Poverty$groupedPivotPlot[[3]]$data %>% mutate(quest='Poverty'),
    topIssues.RacialJustice$groupedPivotPlot[[1]]$data %>% mutate(quest='Racial Justice'),
    topIssues.TaxPolicy$groupedPivotPlot[[4]]$data %>% mutate(quest='Tax Policy'),
    topIssues.Terrorism$groupedPivotPlot[[4]]$data %>% mutate(quest='Terrorism')) %>%
  group_by(quest) %>%
  mutate(meanVal=mean(avgVal))

ggplot(topIssues.Politics, aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=political_view), size=2) +
  facet_wrap(~Group) +
  labs(title='Which of these Issues are Most Important, in Order?',
       x='Average Rank - Smaller Number Means Greater Importance',y='') +
  scale_colour_brewer(palette = 'RdYlBu') +
  scale_x_continuous(breaks=pretty_breaks(8)) +
  guides(colour=guide_legend(title="Political Views")) +
  theme(plot.title=element_text(hjust=.5))

ggplot(topIssues.Politics %>% filter(Group=='Gen Pop'),
       aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=political_view), size=2) +
  facet_wrap(~Group) +
  labs(title='Which of these Issues are Most Important, in Order?',
       x='Average Rank - Smaller Number Means Greater Importance',y='') +
  scale_colour_brewer(palette = 'RdYlBu') +
  scale_x_continuous(breaks=pretty_breaks(8)) +
  guides(colour=guide_legend(title="Political Views")) +
  theme(plot.title=element_text(hjust=.5))

# gender

topIssues.Gender <-
  bind_rows(
    topIssues.AccessHealthcare$groupedPivotPlot[[8]]$data %>% mutate(quest='Access to Healthcare'),
    topIssues.CollegeAffordability$groupedPivotPlot[[7]]$data %>% mutate(quest='College Affordability'),
    topIssues.CountryDebt$groupedPivotPlot[[10]]$data %>% mutate(quest='Federal Debt'),
    topIssues.Education$groupedPivotPlot[[5]]$data %>% mutate(quest='Education'),
    topIssues.GenderEquality$groupedPivotPlot[[7]]$data %>% mutate(quest='Gender Equality'),
    topIssues.Jobs$groupedPivotPlot[[7]]$data %>% mutate(quest='Jobs'),
    topIssues.RacialJustice$groupedPivotPlot[[4]]$data %>% mutate(quest='Racial Justice'),
    topIssues.Terrorism$groupedPivotPlot[[8]]$data %>% mutate(quest='Terrorism')) %>%
  group_by(quest) %>%
  mutate(meanVal=mean(avgVal)) %>%
  filter(sex %in% c('Male','Female','Non-binary'))

ggplot(topIssues.Gender, aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=sex), size=2) +
  facet_wrap(~Group) +
  labs(title='Which of these Issues are Most Important, in Order?',
       x='Average Rank - Smaller Number Means Greater Importance',y='') +
  scale_colour_brewer(palette = 'Set2') +
  scale_x_continuous(breaks=pretty_breaks(8)) +
  guides(colour=guide_legend(title="Gender")) +
  theme(plot.title=element_text(hjust=.5))

ggplot(topIssues.Gender %>% filter(Group=='Gen Pop'), aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=sex), size=2) +
  facet_wrap(~Group) +
  labs(title='Which of these Issues are Most Important, in Order?',
       x='Average Rank - Smaller Number Means Greater Importance',y='') +
  scale_colour_brewer(palette = 'Set2') +
  scale_x_continuous(breaks=pretty_breaks(8)) +
  guides(colour=guide_legend(title="Gender")) +
  theme(plot.title=element_text(hjust=.5))


# Cause Importance --------------------------------------------------------

causes <- apropos('^causeImportance')
checkThis <- 'Group'
for (j in 1:length(causes)) {
  for (i in 1:length(get(causes[j])$pivotPlot)) {
    if (grepl(checkThis,paste(causes[j],i,get(causes[j])$pivotPlot[[i]]$labels$title))) {
      print(paste(causes[j],i,get(causes[j])$pivotPlot[[i]]$labels$title))
    }
  }
}

causeImportance.Ovr <-
  bind_rows(
    causeImportance.DisasterRelief$groupedPivotPlot[[3]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Disaster Relief'),
    tibble(
      avgVal=weighted.mean(
        causeImportance.AnimalWelfare$frequencyPlot$data$outcome,
        causeImportance.AnimalWelfare$frequencyPlot$data$count),
      Group='Animal Welfare'
    ),
    tibble(
      avgVal=weighted.mean(
        causeImportance.Bullying$frequencyPlot$data$outcome,
        causeImportance.Bullying$frequencyPlot$data$count),
      Group='Bullying'
    ),
    tibble(
      avgVal=weighted.mean(
        causeImportance.Environment$frequencyPlot$data$outcome,
        causeImportance.Environment$frequencyPlot$data$count),
      Group='The Environment'
    ),
    tibble(
      avgVal=weighted.mean(
        causeImportance.HomelessnessPoverty$frequencyPlot$data$outcome,
        causeImportance.HomelessnessPoverty$frequencyPlot$data$count),
      Group='Homelessness/Poverty'
    ),
    tibble(
      avgVal=weighted.mean(
        causeImportance.Immigration$frequencyPlot$data$outcome,
        causeImportance.Immigration$frequencyPlot$data$count),
      Group='Immigration'
    ),
    tibble(
      avgVal=weighted.mean(
        causeImportance.IncomeInequality$frequencyPlot$data$outcome,
        causeImportance.IncomeInequality$frequencyPlot$data$count),
      Group='Income Inequality'
    ),
    tibble(
      avgVal=weighted.mean(
        causeImportance.LGBTQRight$frequencyPlot$data$outcome,
        causeImportance.LGBTQRight$frequencyPlot$data$count),
      Group='LGBTQ Rights'
    ),
    tibble(
      avgVal=weighted.mean(
        causeImportance.Mental_Health$frequencyPlot$data$outcome,
        causeImportance.Mental_Health$frequencyPlot$data$count),
      Group='Mental Health'
    ),
    causeImportance.GenderEquality$groupedPivotPlot[[9]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Gender Equality'),
    causeImportance.GunViolence$groupedPivotPlot[[9]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Gun Violence'),
    causeImportance.RacialEquality$groupedPivotPlot[[10]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Racial Equality'),
    tibble(
      avgVal=weighted.mean(
        causeImportance.SexualHarrassmentAssault$frequencyPlot$data$outcome,
        causeImportance.SexualHarrassmentAssault$frequencyPlot$data$count),
      Group='Sexual Harrassment/Assault'
    ),
    tibble(
      avgVal=weighted.mean(
        causeImportance.VoterReg$frequencyPlot$data$outcome,
        causeImportance.VoterReg$frequencyPlot$data$count),
      Group='Voter Registration'
    )
  )

ggplot(causeImportance.Ovr, aes(x=reorder(Group,-avgVal), y=avgVal)) +
  geom_bar(stat='identity', fill='#6ac6b4') +
  geom_text(aes(x=Group,y=avgVal,label=round(avgVal,2)),hjust=-.11,size=3.4) +
  labs(title='How Important are Each of These Causes To You? (Gen Pop)', #TODO: Check language
       x='',y='Average Response') +
  scale_y_continuous(
    breaks=seq(-2,2,1),limits = c(-2,2.2),
    labels = c('Not at all important','Not Important','Neutral',
               'Somewhat Important','Very Important')) +
  theme(
    plot.title=element_text(hjust=.5)
  ) +
  coord_flip()

for (j in 1:length(causes)) {
  for (i in 1:length(get(causes[j])$pivotPlot)) {
    print(paste(causes[j],i,get(causes[j])$pivotPlot[[i]]$labels$title))
  }
}

# political view

checkThis <- 'political_view'
for (j in 1:length(causes)) {
  for (i in 1:length(get(causes[j])$pivotPlot)) {
    if (grepl(checkThis,paste(causes[j],i,get(causes[j])$pivotPlot[[i]]$labels$title))) {
      print(paste(causes[j],i,get(causes[j])$pivotPlot[[i]]$labels$title))
    }
  }
}

causeImport.Politics <-
  bind_rows(
    causeImportance.AnimalWelfare$groupedPivotPlot[[2]]$data %>% mutate(quest='Animal Welfare'),
    causeImportance.Bullying$groupedPivotPlot[[3]]$data %>% mutate(quest='Bullying'),
    causeImportance.Environment$groupedPivotPlot[[4]]$data %>% mutate(quest='The Environment'),
    causeImportance.GenderEquality$groupedPivotPlot[[4]]$data %>% mutate(quest='Gender Equality'),
    causeImportance.GunViolence$groupedPivotPlot[[4]]$data %>% mutate(quest='Gun Violence'),
    causeImportance.HomelessnessPoverty$groupedPivotPlot[[2]]$data %>% mutate(quest='Homelessness/Poverty'),
    causeImportance.Immigration$groupedPivotPlot[[1]]$data %>% mutate(quest='Immigration'),
    causeImportance.IncomeInequality$groupedPivotPlot[[2]]$data %>% mutate(quest='Income Inequality'),
    causeImportance.LGBTQRight$groupedPivotPlot[[5]]$data %>% mutate(quest='LGBTQ Rights'),
    causeImportance.Mental_Health$groupedPivotPlot[[3]]$data %>% mutate(quest='Mental Health'),
    causeImportance.RacialEquality$groupedPivotPlot[[5]]$data %>% mutate(quest='Racial Equality'),
    causeImportance.SexualHarrassmentAssault$groupedPivotPlot[[3]]$data %>% mutate(quest='Sexual Harrassment/Assault'),
    causeImportance.VoterReg$groupedPivotPlot[[3]]$data %>% mutate(quest='Voter Registration')
  ) %>%
  group_by(quest) %>%
  mutate(meanVal=mean(avgVal)) %>% ungroup() %>%
  group_by(Group) %>% mutate(meanBygroup=mean(avgVal))

ggplot(causeImport.Politics, aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=political_view), size=2) +
  facet_wrap(~Group) +
  labs(title='How Important are Each of These Causes?',
       x='Level of Importance',y='') +
  scale_colour_brewer(palette = 'RdYlBu') +
  scale_x_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels = c('Not at all important','Not Important','Neutral',
                                'Somewhat Important','Very Important')) +
  guides(colour=guide_legend(title="Political Views")) +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))

ggplot(causeImport.Politics, aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=political_view), size=2) +
  facet_wrap(~Group) +
  labs(title='How Important are Each of These Causes?',
       x='Level of Importance',y='') +
  scale_colour_brewer(palette = 'RdYlBu') +
  scale_x_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels = c('Not at all important','Not Important','Neutral',
                                'Somewhat Important','Very Important')) +
  guides(colour=guide_legend(title="Political Views")) +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))

ggplot(causeImport.Politics %>% filter(Group=='Gen Pop'), aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=political_view), size=2) +
  facet_wrap(~Group) +
  labs(title='How Important are Each of These Causes?',
       x='Level of Importance',y='') +
  scale_colour_brewer(palette = 'RdYlBu') +
  scale_x_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels = c('Not at all important','Not Important','Neutral',
                                'Somewhat Important','Very Important')) +
  guides(colour=guide_legend(title="Political Views")) +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))

# gender
checkThis <- 'sex'
for (j in 1:length(causes)) {
  for (i in 1:length(get(causes[j])$pivotPlot)) {
    if (grepl(checkThis,paste(causes[j],i,get(causes[j])$pivotPlot[[i]]$labels$title))) {
      print(paste(causes[j],i,get(causes[j])$pivotPlot[[i]]$labels$title))
    }
  }
}

causeImport.Gender <-
  bind_rows(
    causeImportance.AnimalWelfare$groupedPivotPlot[[6]]$data %>% mutate(quest='Animal Welfare'),
    causeImportance.Bullying$groupedPivotPlot[[5]]$data %>% mutate(quest='Bullying'),
    causeImportance.GenderEquality$groupedPivotPlot[[8]]$data %>% mutate(quest='Gender Equality'),
    causeImportance.GunViolence$groupedPivotPlot[[8]]$data %>% mutate(quest='Gun Violence'),
    causeImportance.HomelessnessPoverty$groupedPivotPlot[[7]]$data %>% mutate(quest='Homelessness/Poverty'),
    causeImportance.Immigration$groupedPivotPlot[[5]]$data %>% mutate(quest='Immigration'),
    causeImportance.IncomeInequality$groupedPivotPlot[[7]]$data %>% mutate(quest='Income Inequality'),
    causeImportance.LGBTQRight$groupedPivotPlot[[9]]$data %>% mutate(quest='LGBTQ Rights'),
    causeImportance.Mental_Health$groupedPivotPlot[[6]]$data %>% mutate(quest='Mental Health'),
    causeImportance.RacialEquality$groupedPivotPlot[[9]]$data %>% mutate(quest='Racial Equality'),
    causeImportance.SexualHarrassmentAssault$groupedPivotPlot[[6]]$data %>% mutate(quest='Sexual Harrassment/Assault')
  ) %>%
  group_by(quest) %>%
  mutate(meanVal=mean(avgVal)) %>% ungroup() %>%
  group_by(Group) %>% mutate(meanBygroup=mean(avgVal)) %>%
  filter(sex %in% c('Male','Female','Non-binary'))

ggplot(causeImport.Gender, aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=sex), size=2) +
  facet_wrap(~Group) +
  labs(title='How Important are Each of These Causes?',
       x='Level of Importance',y='') +
  scale_colour_brewer(palette = 'Set2') +
  scale_x_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels = c('Not at all important','Not Important','Neutral',
                                'Somewhat Important','Very Important')) +
  guides(colour=guide_legend(title="Political Views")) +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))

ggplot(causeImport.Gender %>% filter(Group=='Gen Pop'),
       aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=sex), size=2) +
  facet_wrap(~Group) +
  labs(title='How Important are Each of These Causes?',
       x='Level of Importance',y='') +
  scale_colour_brewer(palette = 'Set2') +
  scale_x_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels = c('Not at all important','Not Important','Neutral',
                                'Somewhat Important','Very Important')) +
  guides(colour=guide_legend(title="Political Views")) +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))

# race

causeImport.Race <-
  bind_rows(
    causeImportance.AnimalWelfare$groupedPivotPlot[[4]]$data %>% mutate(quest='Animal Welfare'),
    causeImportance.Environment$groupedPivotPlot[[6]]$data %>% mutate(quest='The Environment'),
    causeImportance.GenderEquality$groupedPivotPlot[[6]]$data %>% mutate(quest='Gender Equality'),
    causeImportance.GunViolence$groupedPivotPlot[[6]]$data %>% mutate(quest='Gun Violence'),
    causeImportance.HomelessnessPoverty$groupedPivotPlot[[4]]$data %>% mutate(quest='Homelessness/Poverty'),
    causeImportance.Immigration$groupedPivotPlot[[3]]$data %>% mutate(quest='Immigration'),
    causeImportance.IncomeInequality$groupedPivotPlot[[5]]$data %>% mutate(quest='Income Inequality'),
    causeImportance.LGBTQRight$groupedPivotPlot[[7]]$data %>% mutate(quest='LGBTQ Rights'),
    causeImportance.RacialEquality$groupedPivotPlot[[7]]$data %>% mutate(quest='Racial Equality'),
    causeImportance.VoterReg$groupedPivotPlot[[5]]$data %>% mutate(quest='Voter Registration')
  ) %>%
  group_by(quest) %>%
  mutate(meanVal=mean(avgVal)) %>% ungroup() %>%
  group_by(Group) %>% mutate(meanBygroup=mean(avgVal)) %>%
  filter(race %in% c('Asian','Black','Hispanic/Latino','White','Multiracial'))

ggplot(causeImport.Race, aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=race), size=2) +
  facet_wrap(~Group) +
  labs(title='How Important are Each of These Causes?',
       x='Level of Importance',y='') +
  scale_colour_brewer(palette = 'Set2') +
  scale_x_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels = c('Not at all important','Not Important','Neutral',
                                'Somewhat Important','Very Important')) +
  guides(colour=guide_legend(title="Political Views")) +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))


# Agree Position ----------------------------------------------------------


positions <- apropos('^agreePosition.')
for (j in 1:length(positions)) {
  for (i in 1:length(get(positions[j])$pivotPlot)) {
    print(paste(positions[j],i,get(positions[j])$pivotPlot[[i]]$labels$title))
  }
}
checkThis <- 'Group'
for (j in 1:length(positions)) {
  for (i in 1:length(get(positions[j])$pivotPlot)) {
    if (grepl(checkThis,paste(positions[j],i,get(positions[j])$pivotPlot[[i]]$labels$title))) {
      print(paste(positions[j],i,get(positions[j])$pivotPlot[[i]]$labels$title))
    }
  }
}

# [1] "agreePosition.BackgroundChecksForGuns 10 Group"
# [1] "agreePosition.BuildTheWall 10 Group"
# [1] "agreePosition.ClimateChangeHappening 10 Group"
# [1] "agreePosition.MuslimsDontNeedCloseTabs 9 Group"
# [1] "agreePosition.RacismNotIssue 10 Group"
# [1] "agreePosition.UndocumentedImmigrantsPathCitizenship 10 Group"
# [1] "agreePosition.WomenExperienceWorkplaceDiscrimination 11 Group"

agreePositions.Ovr <-
  bind_rows(
    agreePosition.BackgroundChecksForGuns$groupedPivotPlot[[10]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Background Checks are Needed for All Guns'),
    tibble(
      avgVal=weighted.mean(
        agreePosition.DrugsProblemInCommunity$frequencyPlot$data$outcome,
        agreePosition.DrugsProblemInCommunity$frequencyPlot$data$count),
      Group='Drugs are Problem in My Community'
    ),
    tibble(
      avgVal=weighted.mean(
        agreePosition.GovernmentProvideHealthcare$frequencyPlot$data$outcome,
        agreePosition.GovernmentProvideHealthcare$frequencyPlot$data$count),
      Group='Government Should Provide All Healthcare'
    ),
    tibble(
      avgVal=weighted.mean(
        agreePosition.iTrustPoliceMyCommunity$frequencyPlot$data$outcome,
        agreePosition.iTrustPoliceMyCommunity$frequencyPlot$data$count),
      Group='I Trust Police in My Community'
    ),
    tibble(
      avgVal=weighted.mean(
        agreePosition.OnlyTwoGenders$frequencyPlot$data$outcome,
        agreePosition.OnlyTwoGenders$frequencyPlot$data$count),
      Group='There are Only Two Genders'
    ),
    tibble(
      avgVal=weighted.mean(
        agreePosition.SexualHarrasmentSignOffSocietalIssue$frequencyPlot$data$outcome,
        agreePosition.SexualHarrasmentSignOffSocietalIssue$frequencyPlot$data$count),
      Group='Sexual Harassment is a Sign of Societal Issues'
    ),
    agreePosition.BuildTheWall$groupedPivotPlot[[10]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Need Wall on Mexican Border'),
    agreePosition.ClimateChangeHappening$groupedPivotPlot[[10]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Climate Change is Man Made'),
    agreePosition.MuslimsDontNeedCloseTabs$groupedPivotPlot[[9]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Muslims Dont Need Additional Scrutiny'),
    agreePosition.RacismNotIssue$groupedPivotPlot[[10]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Racism is No Longer an Issue'),
    agreePosition.UndocumentedImmigrantsPathCitizenship$groupedPivotPlot[[10]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Undocumented Immigrants Should Have Path to Citizenship'),
    agreePosition.WomenExperienceWorkplaceDiscrimination$groupedPivotPlot[[11]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Women Experience Discrimination in Workplace'),
    tibble(
      avgVal=weighted.mean(
        agreePosition.WeedLawsTooStrict$frequencyPlot$data$outcome,
        agreePosition.WeedLawsTooStrict$frequencyPlot$data$count),
      Group='Marijuana Laws are Too Strict'
    )
  )

ggplot(agreePositions.Ovr, aes(x=reorder(Group,-avgVal), y=avgVal)) +
  geom_bar(stat='identity', fill='#6ac6b4') +
  geom_text(aes(x=Group,y=avgVal,label=round(avgVal,2)),size=3.4,hjust=-.11) +
  labs(title='Do You Agree With the Following Positions? (Gen Pop)', #TODO: Check language
       x='',y='Average Response') +
  scale_y_continuous(
    breaks=seq(-2,2,1),limits = c(-2,2.2),
    labels = c('Strongly Disagree','Disagree','Neutral',
               'Agree','Strongly Agree')) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  theme(
    plot.title=element_text(hjust=.5)#,
    # axis.text.x = element_text(angle=0,hjust=1)
  ) + coord_flip()

# Background checks

gunsPolitics <-
  ggplot(agreePosition.BackgroundChecksForGuns$groupedPivotPlot[[4]]$data,
       aes(x=political_view, y=avgVal, fill=Group)) +
  geom_bar(stat='identity', position='dodge',alpha=.8) +
  geom_smooth(method='lm', aes(group=Group, colour=Group), se=F, linetype='dashed') +
  labs(title='Background Checks are Needed for All Guns', y='',x='') +
  scale_color_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  theme(plot.title = element_text(hjust=.5)) +
  scale_y_continuous(
    breaks=seq(-2,2,1),limits = c(-2,2.2),
    labels = c('Strongly Disagree','Disagree','Neutral',
               'Agree','Strongly Agree'))

checkThis <- 'political_view'
for (j in 1:length(positions)) {
  for (i in 1:length(get(positions[j])$pivotPlot)) {
    if (grepl(checkThis,paste(positions[j],i,get(positions[j])$pivotPlot[[i]]$labels$title))) {
      print(paste(positions[j],i,get(positions[j])$pivotPlot[[i]]$labels$title))
    }
  }
}

agreePosition.Politics <-
  bind_rows(
    agreePosition.BackgroundChecksForGuns$groupedPivotPlot[[4]]$data %>% mutate(quest='Background Checks are Needed for All Guns'),
    agreePosition.BuildTheWall$groupedPivotPlot[[5]]$data %>% mutate(quest='Need Wall on Mexican Border'),
    agreePosition.ClimateChangeHappening$groupedPivotPlot[[5]]$data %>% mutate(quest='Climate Change is Man Made'),
    agreePosition.GovernmentProvideHealthcare$groupedPivotPlot[[4]]$data %>% mutate(quest='Government Should Provide All Healthcare'),
    agreePosition.iTrustPoliceMyCommunity$groupedPivotPlot[[4]]$data %>% mutate(quest='I Trust Police in My Community'),
    agreePosition.MuslimsDontNeedCloseTabs$groupedPivotPlot[[5]]$data %>% mutate(quest='Muslims Dont Need Additional Scrutiny'),
    agreePosition.OnlyTwoGenders$groupedPivotPlot[[4]]$data %>% mutate(quest='There are Only Two Genders'),
    agreePosition.RacismNotIssue$groupedPivotPlot[[5]]$data %>% mutate(quest='Racism is No Longer an Issue'),
    agreePosition.SexualHarrasmentSignOffSocietalIssue$groupedPivotPlot[[3]]$data %>% mutate(quest='Sexual Harassment is a Sign of Societal Issues'),
    agreePosition.UndocumentedImmigrantsPathCitizenship$groupedPivotPlot[[5]]$data %>% mutate(quest='Undocumented Immigrants Should Have Path to Citizenship'),
    agreePosition.WeedLawsTooStrict$groupedPivotPlot[[4]]$data %>% mutate(quest='Marijuana Laws are Too Strict'),
    agreePosition.WomenExperienceWorkplaceDiscrimination$groupedPivotPlot[[4]]$data %>% mutate(quest='Women Experience Discrimination in Workplace')
  ) %>%
  group_by(quest) %>%
  mutate(meanVal=mean(avgVal)) %>% ungroup() %>%
  group_by(Group) %>% mutate(meanBygroup=mean(avgVal))

ggplot(agreePosition.Politics, aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=political_view), size=2) +
  facet_wrap(~Group) +
  labs(title='How Much Do You Agree With the Following Positions',
       x='',y='') +
  scale_colour_brewer(palette = 'RdYlBu') +
  scale_x_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels =c('Strongly Disagree','Disagree','Neutral',
                               'Agree','Strongly Agree')) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
  guides(colour=guide_legend(title="Political Views")) +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))

ggplot(agreePosition.Politics %>% filter(Group=='Gen Pop'),
       aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=political_view), size=2) +
  facet_wrap(~Group) +
  labs(title='How Much Do You Agree With the Following Positions',
       x='',y='') +
  scale_colour_brewer(palette = 'RdYlBu') +
  scale_x_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels =c('Strongly Disagree','Disagree','Neutral',
                               'Agree','Strongly Agree')) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
  guides(colour=guide_legend(title="Political Views")) +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))


ggplot(agreePosition.SexualHarrasmentSignOffSocietalIssue$groupedPivotPlot[[5]]$data,
       aes(x=age, y=avgVal, fill=Group)) +
  geom_bar(stat='identity', position='dodge',alpha=.8) +
  geom_smooth(method='lm', aes(group=Group, colour=Group), se=F, linetype='dashed') +
  labs(title='Sexual Harassment is a Sign of Societal Issues', y='',x='') +
  scale_color_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  theme(plot.title = element_text(hjust=.5)) +
  scale_x_continuous(breaks=seq(13,25,1)) +
  scale_y_continuous(
    breaks=seq(-2,2,1),limits = c(-2,2.2),
    labels = c('Strongly Disagree','Disagree','Neutral',
               'Agree','Strongly Agree'))

bind_rows(
  agreePosition.iTrustPoliceMyCommunity$groupedPivotPlot[[6]]$data,
  agreePosition.iTrustPoliceMyCommunity$groupedPivotPlot[[7]]$data,
  agreePosition.iTrustPoliceMyCommunity$groupedPivotPlot[[8]]$data,
  agreePosition.iTrustPoliceMyCommunity$groupedPivotPlot[[11]]$data
)

policeEdu <-
  ggplot(agreePosition.iTrustPoliceMyCommunity$groupedPivotPlot[[6]]$data %>%
           filter(Group=='Gen Pop'),
         aes(x=parental_education, y=avgVal, fill='#6ac6b4')) +
  geom_bar(stat='identity', position='dodge',alpha=.8) +
  labs(title='Parental Education', y='',x='') +
  scale_color_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  theme(plot.title = element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1),
        legend.position = 'none') +
  scale_y_continuous(
    breaks=seq(-2,2,1),limits = c(-2,2.2),
    labels = c('Strongly Disagree','Disagree','Neutral',
               'Agree','Strongly Agree'))

policeRegion <-
  ggplot(agreePosition.iTrustPoliceMyCommunity$groupedPivotPlot[[7]]$data %>%
           filter(Group=='Gen Pop'),
         aes(x=region, y=avgVal, fill='#6ac6b4')) +
  geom_bar(stat='identity', position='dodge',alpha=.8) +
  labs(title='Geography', y='',x='') +
  scale_color_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  theme(plot.title = element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1),
        legend.position = 'none') +
  scale_y_continuous(
    breaks=seq(-2,2,1),limits = c(-2,2.2),
    labels = c('Strongly Disagree','Disagree','Neutral',
               'Agree','Strongly Agree'))

policeRace <-
  ggplot(agreePosition.iTrustPoliceMyCommunity$groupedPivotPlot[[8]]$data %>%
           filter(race %in% c('Asian','Black','Hispanic/Latino','White') &
                    Group=='Gen Pop'),
         aes(x=race, y=avgVal, fill='#6ac6b4')) +
  geom_bar(stat='identity', position='dodge',alpha=.8) +
  labs(title='Race', y='',x='') +
  scale_color_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  theme(plot.title = element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1),
        legend.position = 'none') +
  scale_y_continuous(
    breaks=seq(-2,2,1),limits = c(-2,2.2),
    labels = c('Strongly Disagree','Disagree','Neutral',
               'Agree','Strongly Agree'))

policeGender <-
  ggplot(agreePosition.iTrustPoliceMyCommunity$groupedPivotPlot[[11]]$data %>%
           filter(sex %in% c('Male','Female','Non-binary') & Group=='Gen Pop'),
         aes(x=sex, y=avgVal, fill='#6ac6b4')) +
  geom_bar(stat='identity', position='dodge',alpha=.8) +
  labs(title='Gender', y='',x='') +
  scale_color_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  theme(plot.title = element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1),
        legend.position = 'none') +
  scale_y_continuous(
    breaks=seq(-2,2,1),limits = c(-2,2.2),
    labels = c('Strongly Disagree','Disagree','Neutral',
               'Agree','Strongly Agree'))

grid.arrange(policeEdu, policeRace,policeRegion, policeGender, ncol=2,
             top=textGrob("I Trust the Police in My Community",gp=gpar(fontsize=20)))


# Impact attitudes --------------------------------------------------------

attitudes <- apropos('^impAct.')
for (j in 1:length(positions)) {
  for (i in 1:length(get(positions[j])$pivotPlot)) {
    print(paste(positions[j],i,get(positions[j])$pivotPlot[[i]]$labels$title))
  }
}
checkThis <- 'Group'
for (j in 1:length(attitudes)) {
  for (i in 1:length(get(attitudes[j])$pivotPlot)) {
    if (grepl(checkThis,paste(attitudes[j],i,get(attitudes[j])$pivotPlot[[i]]$labels$title))) {
      print(paste(attitudes[j],i,get(attitudes[j])$pivotPlot[[i]]$labels$title))
    }
  }
}
whichOnes <- c('political_view','age','attend_religious_services_freq')
impAct.ovr <-
  bind_rows(
    impAct.effortUnderstandPerspectives$groupedPivotPlot[[8]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='I Make an Effort to Understand Other Perspectives'),
    impAct.iCanCollaborate$groupedPivotPlot[[6]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='I Can Collaborate Well With Others'),
    impAct.myParticipationMatters$groupedPivotPlot[[8]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='My Participation in Local Issues Matters'),
    impAct.partOfSocialMovement$groupedPivotPlot[[11]]$data %>%
      filter(Group=='Gen Pop') %>%
      mutate(Group='Im a Part of a Social Movement'),
    tibble(
      avgVal=weighted.mean(
        impAct.comfortAssertingMyself$frequencyPlot$data$outcome,
        impAct.comfortAssertingMyself$frequencyPlot$data$count),
      Group='Im Comfortable Asserting Myself to Advocate for an Issue'
    ),
    tibble(
      avgVal=weighted.mean(
        impAct.exposedOtherOpinions$frequencyPlot$data$outcome,
        impAct.exposedOtherOpinions$frequencyPlot$data$count),
      Group='Im Often Exposed to Opinions/Worldviews Different From My Own'
    ),
    tibble(
      avgVal=weighted.mean(
        impAct.iCanSolveProblems$frequencyPlot$data$outcome,
        impAct.iCanSolveProblems$frequencyPlot$data$count),
      Group='I Can Solve Social Problems'
    ),
    tibble(
      avgVal=weighted.mean(
        impAct.iHaveConfidence$frequencyPlot$data$outcome,
        impAct.iHaveConfidence$frequencyPlot$data$count),
      Group='I Have Confidence in Myself'
    ),
    tibble(
      avgVal=weighted.mean(
        impAct.iTakeActionsIssues$frequencyPlot$data$outcome,
        impAct.iTakeActionsIssues$frequencyPlot$data$count),
      Group='I Often Take Action on Issues I Learn About'
    )
  ) %>%
  mutate(
    Level =
      case_when(
        Group %in% c('I Have Confidence in Myself',
                     'Im Comfortable Asserting Myself to Advocate for an Issue',
                     'I Often Take Action on Issues I Learn About'
                     ) ~ 'Personal',
        Group %in% c('I Make an Effort to Understand Other Perspectives',
                     'I Can Collaborate Well With Others',
                     'Im Often Exposed to Opinions/Worldviews Different From My Own'
                     ) ~ 'Social Circle',
        Group %in% c('My Participation in Local Issues Matters',
                     'Im a Part of a Social Movement',
                     'I Can Solve Social Problems'
                     ) ~ 'Societal',
        TRUE ~ ''
      )
  )

impAct.ovr.p <-
  ggplot(impAct.ovr, aes(x=reorder(Group,-avgVal), y=avgVal, fill=Level)) +
  geom_bar(stat='identity') +
  geom_text(aes(x=Group,y=avgVal,label=round(avgVal,2)),size=3.4,hjust=-.11) +
  labs(title='Do You Agree With the Following Positions? (Gen Pop)', #TODO: Check language
       x='',y='Average Response') +
  scale_y_continuous(
    breaks=seq(-2,2,1),limits = c(-2,2.2),
    labels = c('Strongly Disagree','Disagree','Neutral',
               'Agree','Strongly Agree')) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_fill_brewer(palette = 'Set2') +
  theme(plot.title=element_text(hjust=.5)) + coord_flip()

checkThis <- 'political_view'
for (j in 1:length(attitudes)) {
  for (i in 1:length(get(attitudes[j])$pivotPlot)) {
    if (grepl(checkThis,paste(attitudes[j],i,get(attitudes[j])$pivotPlot[[i]]$labels$title))) {
      print(paste(attitudes[j],i,get(attitudes[j])$pivotPlot[[i]]$labels$title))
    }
  }
}

# politics

impAct.Politics <-
  bind_rows(
    impAct.effortUnderstandPerspectives$groupedPivotPlot[[4]]$data %>% mutate(quest='I Make an Effort to Understand Other Perspectives'),
    impAct.exposedOtherOpinions$groupedPivotPlot[[3]]$data %>% mutate(quest='Im Often Exposed to Opinions/Worldviews Different From My Own'),
    impAct.iCanSolveProblems$groupedPivotPlot[[3]]$data %>% mutate(quest='I Can Solve Social Problems'),
    impAct.iHaveConfidence$groupedPivotPlot[[3]]$data %>% mutate(quest='I Have Confidence in Myself'),
    impAct.iTakeActionsIssues$groupedPivotPlot[[3]]$data %>% mutate(quest='I Often Take Action on Issues I Learn About'),
    impAct.myParticipationMatters$groupedPivotPlot[[4]]$data %>% mutate(quest='My Participation in Local Issues Matters'),
    impAct.partOfSocialMovement$groupedPivotPlot[[4]]$data %>% mutate(quest='Im a Part of a Social Movement')
   ) %>%
  group_by(quest) %>%
  mutate(meanVal=mean(avgVal)) %>% ungroup() %>%
  group_by(Group) %>% mutate(meanBygroup=mean(avgVal))

ggplot(impAct.Politics, aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=political_view), size=2) +
  facet_wrap(~Group) +
  labs(title='How Much Do You Agree With the Following Positions',
       x='',y='') +
  scale_colour_brewer(palette = 'RdYlBu') +
  scale_x_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels =c('Strongly Disagree','Disagree','Neutral',
                               'Agree','Strongly Agree')) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
  guides(colour=guide_legend(title="Political Views")) +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))

# age

checkThis <- 'age'
for (j in 1:length(attitudes)) {
  for (i in 1:length(get(attitudes[j])$pivotPlot)) {
    if (grepl(checkThis,paste(attitudes[j],i,get(attitudes[j])$pivotPlot[[i]]$labels$title))) {
      print(paste(attitudes[j],i,get(attitudes[j])$pivotPlot[[i]]$labels$title))
    }
  }
}
impAct.Age <-
  bind_rows(
    impAct.comfortAssertingMyself$groupedPivotPlot[[3]]$data %>% mutate(quest='Im Comfortable Asserting Myself to Advocate for an Issue'),
    impAct.effortUnderstandPerspectives$groupedPivotPlot[[7]]$data %>% mutate(quest='I Make an Effort to Understand Other Perspectives'),
    impAct.exposedOtherOpinions$groupedPivotPlot[[5]]$data %>% mutate(quest='Im Often Exposed to Opinions/Worldviews Different From My Own'),
    impAct.iCanCollaborate$groupedPivotPlot[[5]]$data %>% mutate(quest='I Can Collaborate Well With Others'),
    impAct.iCanSolveProblems$groupedPivotPlot[[6]]$data %>% mutate(quest='I Can Solve Social Problems'),
    impAct.iHaveConfidence$groupedPivotPlot[[6]]$data %>% mutate(quest='I Have Confidence in Myself'),
    impAct.iTakeActionsIssues$groupedPivotPlot[[5]]$data %>% mutate(quest='I Often Take Action on Issues I Learn About'),
    impAct.myParticipationMatters$groupedPivotPlot[[7]]$data %>% mutate(quest='My Participation in Local Issues Matters'),
    impAct.partOfSocialMovement$groupedPivotPlot[[8]]$data %>% mutate(quest='Im a Part of a Social Movement')
  ) %>%
  group_by(quest) %>%
  mutate(meanVal=mean(avgVal)) %>% ungroup() %>%
  group_by(Group) %>% mutate(meanBygroup=mean(avgVal))

ggplot(impAct.Age, aes(x=age, y=avgVal, colour=quest)) +
  geom_point() + geom_line(aes(group=quest)) +
  facet_wrap(~Group,ncol=2) +
  scale_colour_brewer(name='',palette='Set3',labels=function(x) str_wrap(x, width = 25)) +
  scale_y_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels =c('Strongly Disagree','Disagree','Neutral',
                               'Agree','Strongly Agree')) +
  scale_x_continuous(breaks=seq(13,25,1)) +
  theme(legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
        plot.title=element_text(hjust=.5)) +
  labs(x='Age',y='',title='How Much Do You Agree With the Following Positions')

# attend_religious_services_freq
checkThis <- 'attend_religious_services_freq'
for (j in 1:length(attitudes)) {
  for (i in 1:length(get(attitudes[j])$pivotPlot)) {
    if (grepl(checkThis,paste(attitudes[j],i,get(attitudes[j])$pivotPlot[[i]]$labels$title))) {
      print(paste(attitudes[j],i,get(attitudes[j])$pivotPlot[[i]]$labels$title))
    }
  }
}

impAct.Religion <-
  bind_rows(
    impAct.exposedOtherOpinions$groupedPivotPlot[[2]]$data %>% mutate(quest='Im Often Exposed to Opinions/Worldviews Different From My Own'),
    impAct.iCanCollaborate$groupedPivotPlot[[4]]$data %>% mutate(quest='I Can Collaborate Well With Others'),
    impAct.iCanSolveProblems$groupedPivotPlot[[2]]$data %>% mutate(quest='I Can Solve Social Problems'),
    impAct.iHaveConfidence$groupedPivotPlot[[2]]$data %>% mutate(quest='I Have Confidence in Myself'),
    impAct.iTakeActionsIssues$groupedPivotPlot[[2]]$data %>% mutate(quest='I Often Take Action on Issues I Learn About'),
    impAct.partOfSocialMovement$groupedPivotPlot[[3]]$data %>% mutate(quest='Im a Part of a Social Movement')
  ) %>%
  group_by(quest) %>%
  mutate(meanVal=mean(avgVal)) %>% ungroup() %>%
  group_by(Group) %>% mutate(meanBygroup=mean(avgVal)) %>%
  mutate(
    Level =
      case_when(
        quest %in% c('I Have Confidence in Myself',
                     'Im Comfortable Asserting Myself to Advocate for an Issue',
                     'I Often Take Action on Issues I Learn About'
        ) ~ 'Personal',
        quest %in% c('I Make an Effort to Understand Other Perspectives',
                     'I Can Collaborate Well With Others',
                     'Im Often Exposed to Opinions/Worldviews Different From My Own'
        ) ~ 'Social Circle',
        quest %in% c('My Participation in Local Issues Matters',
                     'Im a Part of a Social Movement',
                     'I Can Solve Social Problems'
        ) ~ 'Societal',
        TRUE ~ ''
      )
  )

ggplot(impAct.Religion,
       aes(x=attend_religious_services_freq, y=avgVal, colour=quest)) +
  geom_point() + geom_line(aes(group=quest)) +
  facet_wrap(~Group,ncol=2) +
  scale_colour_brewer(name='',palette='Set3',labels=function(x) str_wrap(x, width = 25)) +
  scale_y_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels =c('Strongly Disagree','Disagree','Neutral',
                               'Agree','Strongly Agree')) +
  theme(legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
        plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30, hjust=1)) +
  labs(x='Frequency of Religious Service Attendance',y='',
       title='How Much Do You Agree With the Following Positions')

ggplot(impAct.Religion %>% filter(Group=='Gen Pop' & !grepl('Often',quest)),
       aes(x=attend_religious_services_freq, y=avgVal, colour=quest)) +
  geom_point(aes(shape=Level), size=3) + #geom_line(aes(group=quest)) +
  geom_smooth(aes(group=quest), se=F) +
  scale_colour_brewer(name='',palette='Set2',labels=function(x) str_wrap(x, width = 25)) +
  scale_y_continuous(breaks=seq(-1,2,1), limits = c(-1,2),
                     labels =c('Disagree','Neutral',
                               'Agree','Strongly Agree')) +
  theme(legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
        plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30, hjust=1)) +
  labs(x='Frequency of Religious Service Attendance',y='',title='Gen Pop')
# DS Impact ---------------------------------------------------------------

# actions

sinceDS <- apropos('^sinceDS.')

since.ds.actions <-
  c('sinceDS.startedSocialJusticeOrg','sinceDS.persistThroughChallenges',
    'sinceDS.PickSchoolCourses','sinceDS.continuedActivityAfterCampaign',
    'sinceDS.learnedOrganizeOthers','sinceDS.learnedAnalyzeSolveProblems')

since.ds.actions.dat <-
  bind_rows(
    sinceDS.startedSocialJusticeOrg$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='Started My Own Social Justice Org/Club'),
    sinceDS.persistThroughChallenges$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='Learned How to Persist Through Challenges'),
    sinceDS.PickSchoolCourses$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='Picked Different School Courses'),
    sinceDS.continuedActivityAfterCampaign$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='Continued Activites After Campaign Ended'),
    sinceDS.learnedOrganizeOthers$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='Learned How to Organize People to Work on a Cause'),
    sinceDS.learnedAnalyzeSolveProblems$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='Learned How to Analyze Problems and Implement Creative Solutions'),
    sinceDS.NoneOfThese$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='None of These')
  ) %>%
  mutate(pct = count/sum(sinceDS.amPartOfSocialMovement$frequencyPlot$data$count))

ggplot(since.ds.actions.dat, aes(x=reorder(quest, pct), y=pct)) +
  geom_bar(stat='identity', fill='#6ac6b4') +
  geom_text(aes(label=percent(pct)), angle=-90, nudge_y = .0075, size=3.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_y_continuous(breaks=seq(0,.4,.05), labels = percent(seq(0,.4,.05))) +
  theme(plot.title=element_text(hjust=.5)) +
  labs(title='Since Engaging with DoSomething I...', y='Percent Yes', x='') +
  coord_flip()

plannedSocialImpactActivity$pivotPlot[[2]] +
  scale_y_continuous(
    breaks=seq(-2,2,1),
    labels=c(    'Would Never Do',
                 'Not Done, Dont Know if Ever',
                 'Not Done But Might',
                 'Done in Past',
                 'Done Past Year'),
    limits=c(-2,2)
  ) +
  labs(title='Planned a Social Impact Activity') +
  theme(plot.title=element_text(hjust=.5))

# attitudes

since.ds.attitudes <-
  sinceDS[which(!sinceDS %in% since.ds.actions)]

since.ds.attitudes.dat <-
  bind_rows(
    sinceDS.amPartOfSocialMovement$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='I Feel I am a Part of a Social Movement'),
    sinceDS.considerEffectActionsOnOthers$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='I Consider the Effect of My Actions on Others'),
    sinceDS.easyToImpactSociety$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='Believe it is Easy to Positively Impact Society'),
    sinceDS.effortUnderstandPerspectives$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='I Make an Effort to Understand Others Perspectives'),
    sinceDS.haveToolsToTakeAction$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='I Have the Tools I Need to Take Action on Social Issues'),
    sinceDS.iCanAccomplishGoals$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='I Can Accomplish My Goals'),
    sinceDS.iCanSolveProblems$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='I Can Solve Social Problems'),
    sinceDS.iHaveConfidence$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='I Have More Confidence in Myself'),
    sinceDS.learnedCollaborateOthers$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='I Learned How to Collaborate Effectively With Others'),
    sinceDS.passionateNewCause$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='I Discovered a Social Cause Im Passionate About'),
    sinceDS.thinkLocalIssuesMatter$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='I Feel My Participation in Local Issues Matters'),
    sinceDS.NoneOfThese$frequencyPlot$data %>%
      filter(outcome==1) %>%
      mutate(quest='None of These')
  ) %>%
  mutate(pct = count/sum(sinceDS.amPartOfSocialMovement$frequencyPlot$data$count))

ggplot(since.ds.attitudes.dat, aes(x=reorder(quest, pct), y=pct)) +
  geom_bar(stat='identity', fill='#6ac6b4') +
  geom_text(aes(label=percent(pct)), angle=-90, nudge_y = .0075, size=2.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_y_continuous(breaks=seq(0,.7,.05), labels = percent(seq(0,.7,.05))) +
  theme(plot.title=element_text(hjust=.5)) +
  labs(title='Since Engaging with DoSomething I...', y='Percent Yes', x='') +
  coord_flip()

impAct.partOfSocialMovement$pivotPlot[[1]] +
  scale_y_continuous(
    breaks=seq(-2,2,1),
    labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'),
    limits=c(-2,2)
  ) +
  labs(title='I Am a Part of a Social Movement') +
  theme(plot.title=element_text(hjust=.5))



ggplot(issuesTakenAction.genpop$pivotPlots$political_view$data,
       aes(x=reorder(variable, -value), y=value)) +
  geom_point(aes(color=political_view)) +
  coord_flip()
