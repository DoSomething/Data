
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
  stylePickOneList(set, sex, race, Group)$Facetted +
  labs(title='Gender & Race Comparison') +
  guides(fill=guide_legend(title="Race"))
ggsave(plot=genderRaceGroup, 'Visuals/genderRaceGroup.png', width = 10, height = 6)


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
  labs(x='Influencer', y='How Influential',
       title='How Influential is Each of These Items When Purchasing a Product',
       caption=
         "Influence is the average value where -2 is 'Not at all Influential', 0 is 'Neutral', and 2 is 'Very Influential'") +
  theme(plot.title = element_text(hjust=.5))
ggsave(plot=purchaseDecision.p, 'Visuals/purchaseDecision.png', width = 10, height = 6)

purchaseDecision.politics <-
  purchaseInfluence.CompanyReputationValues$pivotPlot[[1]] +
  labs(title = 'Pivoted by Political Views', y = 'How Influential') +
  theme(plot.title=element_text(hjust=.5, size = 10))

purchaseDecision.Gender <-
  purchaseInfluence.CompanyReputationValues$pivotPlot[[4]] +
  labs(title='Pivoted by Gender') +
  theme(plot.title=element_text(hjust=.5, size = 10))

grid.arrange(purchaseDecision.politics, purchaseDecision.Gender,
             top='Influence of Brand Values on Purchasing Decisions', ncol=2)

willingPayMoreBrandValues$frequencyPlot +
  labs(title='How Much More Are You Willing to Pay For a Brand Whose Values Align With Your Own?',
       x='Amount',y='Count') + theme(plot.title=element_text(hjust=.5))
myPurchaseSupportCauseMakesImpact$frequencyPlot +
  labs(title='I Feel My Purchases are Making an Impact When the Branch Supports a Cause I Believe In',
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
       title='How Familiar Are You With These Organizations',
       caption=
         "Awareness is the average value where -2 is 'Not at all Familiar', 0 is 'Neutral', and 2 is 'Very Familiar'") +
  theme(plot.title = element_text(hjust=.5))

nonprofitAwareness.DoSomething$frequencyPlot +
  labs(title='How Familiar Are You With DoSomething.org', y='Count',x='') +
  theme(plot.title = element_text(hjust = .5))

grid.arrange(
  nonprofitAwareness.DoSomething$pivotPlot[[3]] + ggtitle('DoSomething') + ylim(c(-1.75,.1)) + theme(plot.title = element_text(hjust=.5)),
  nonprofitAwareness.4H$pivotPlot[[7]] + ggtitle('4H') + ylim(c(-1.75,.1)) + theme(plot.title = element_text(hjust=.5)),
  nonprofitAwareness.DonorsChoose$pivotPlot[[4]] + ggtitle('Donors Choose') + ylim(c(-1.75,.1)) + theme(plot.title = element_text(hjust=.5)),
  nonprofitAwareness.ChangeDotOrg$pivotPlot[[6]] + ggtitle('Change.Org') + ylim(c(-1.75,.1)) + theme(plot.title = element_text(hjust=.5)),
  nonprofitAwareness.KeyClub$pivotPlot[[4]] + ggtitle('Key Club') + ylim(c(-1.75,.1)) + theme(plot.title = element_text(hjust=.5)),
  nonprofitAwareness.WeDotOrg$pivotPlot[[5]] + ggtitle('We.Org') + ylim(c(-1.75,.1)) + theme(plot.title = element_text(hjust=.5)),
  ncol=3, top='How Familiar Are You with These Organizations', bottom='X-Axis is Age, Y-Axis is Familiarity'
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
  scale_y_continuous(breaks=pretty_breaks(7)) +
  labs(x='Race',y='Famliarity',title='Familiarity by Race') +
  theme(plot.title = element_text(hjust=.5))

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
  geom_text(stat='count',aes(label=..count..), vjust=-.7, size=3) +
  geom_text(data=tibble(x=9.5,y=145),aes(x=x,y=y, label=percent((69+112)/nrow(npsBreakdown$data)))) +
  geom_text(data=tibble(x=3,y=145),aes(x=x,y=y, label=percent((14+9+16+26+50+111+106)/nrow(npsBreakdown$data)))) +
  geom_text(data=tibble(x=7.5,y=145),aes(x=x,y=y, label=percent((132+124)/nrow(npsBreakdown$data))))

# Willingness to take action  ----------------------------------------------


whenActionsPolitics <-
  bind_rows(
    politicalEventsProtests$pivotPlot[[3]]$data %>% mutate(quest='Attended Protest'),
    takenActionEncouragedOnline$pivotPlot[[5]]$data %>% mutate(quest='Encouraged Others to Take Action Online'),
    contactedPolitician$pivotPlot[[5]]$data %>% mutate(quest='Contacted Politician'),
    signedOnlinePetition$pivotPlot[[5]]$data %>% mutate(quest='Signed a Petition Online'),
    participateVolunteerOrg$pivotPlot[[5]]$data %>% mutate(quest='Participated in Volunteer Org'),
    startedDiscussionPolitics$pivotPlot[[5]]$data %>% mutate(quest='Started Classroom Political Discussion'),
    purchasedBrandToSupportIssue$pivotPlot[[4]]$data %>% mutate(quest='Purchased Brand to Support Cause'),
    boycottedBrandForIssue$pivotPlot[[3]]$data %>% mutate(quest='Boycotted a Brand'),
    # stoodUpToBullyForSomeone
    leaderInClub$pivotPlot[[5]]$data %>% mutate(quest='Held Leadership Role in Club'),
    collectItemsForHomeless$pivotPlot[[4]]$data %>% mutate(quest='Collected Items for Homeless'),
    researchSocialIssueOutsideSchool$pivotPlot[[4]]$data %>% mutate(quest='Researched Social Issue Outside School'),
    engagedCompanyToAdvocate$pivotPlot[[3]]$data %>% mutate(quest='Engaged Company to Advocate for Cause'),
    plannedSocialImpactActivity$pivotPlot[[5]]$data %>% mutate(quest='Planned an Activity for Social Impact'),
    donateTimeMoneyToCause$pivotPlot[[5]]$data %>% mutate(quest='Donated Time/Money to Cause'),
    createdPetition$pivotPlot[[2]]$data %>% mutate(quest='Created a Petition'),
    startedCampaignToSolveProblem$pivotPlot[[3]]$data %>% mutate(quest='Started a Campaign'),
    beenPartOfCampaign$pivotPlot[[5]]$data %>% mutate(quest='Took Part in a Campaign')
  ) %>%
  group_by(quest) %>%
  mutate(meanVal = mean(avgVal))


ggplot(whenActionsPolitics, aes(x=avgVal, y=reorder(quest, -meanVal), color=political_view)) +
  geom_point() +
  scale_x_continuous(
    labels=c('-1'='Unlikely To do','0'='Might Do','1'='Did Long Ago','2'='Done Past Year'),
    breaks=c(-1,0,1,2), limits=c(-1,2)
    ) +
  labs(y='',x='',title='Which of the Following Actions Have You Taken & When?') +
  scale_colour_brewer(palette = 'RdYlBu',name='Political Views') +
  theme(plot.title=element_text(hjust=.5))


whenActionsGender <-
  bind_rows(
    takenActionEncouragedOnline$pivotPlot[[9]]$data %>% mutate(quest='Encouraged Others to Take Action Online'),
    contactedPolitician$pivotPlot[[10]]$data %>% mutate(quest='Contacted Politician'),
    signedOnlinePetition$pivotPlot[[10]]$data %>% mutate(quest='Signed a Petition Online'),
    participateVolunteerOrg$pivotPlot[[12]]$data %>% mutate(quest='Participated in Volunteer Org'),
    purchasedBrandToSupportIssue$pivotPlot[[9]]$data %>% mutate(quest='Purchased Brand to Support Cause'),
    boycottedBrandForIssue$pivotPlot[[6]]$data %>% mutate(quest='Boycotted a Brand'),
    collectItemsForHomeless$pivotPlot[[6]]$data %>% mutate(quest='Collected Items for Homeless'),
    researchSocialIssueOutsideSchool$pivotPlot[[9]]$data %>% mutate(quest='Researched Social Issue Outside School'),
    engagedCompanyToAdvocate$pivotPlot[[7]]$data %>% mutate(quest='Engaged Company to Advocate for Cause'),
    plannedSocialImpactActivity$pivotPlot[[10]]$data %>% mutate(quest='Planned an Activity for Social Impact'),
    donateTimeMoneyToCause$pivotPlot[[11]]$data %>% mutate(quest='Donated Time/Money to Cause'),
    createdPetition$pivotPlot[[5]]$data %>% mutate(quest='Created a Petition'),
    startedCampaignToSolveProblem$pivotPlot[[7]]$data %>% mutate(quest='Started a Campaign'),
    beenPartOfCampaign$pivotPlot[[9]]$data %>% mutate(quest='Took Part in a Campaign')
  ) %>%
  group_by(quest) %>%
  mutate(meanVal = mean(avgVal)) %>%
  filter(sex %in% c('Male','Female','Non-binary'))

ggplot(whenActionsGender, aes(x=avgVal, y=reorder(quest, -meanVal), color=sex)) +
  geom_point() +
  scale_x_continuous(
    labels=c('-1'='Unlikely To do','0'='Might Do','1'='Did Long Ago','2'='Done Past Year'),
    breaks=c(-1,0,1,2), limits=c(-1,2)
  ) +
  labs(y='',x='',title='') +
  scale_colour_brewer(palette = 'Set2',name='Gender')

whenActionsRace <-
  bind_rows(
    politicalEventsProtests$pivotPlot[[7]]$data %>% mutate(quest='Attended Protest'),
    takenActionEncouragedOnline$pivotPlot[[7]]$data %>% mutate(quest='Encouraged Others to Take Action Online'),
    contactedPolitician$pivotPlot[[8]]$data %>% mutate(quest='Contacted Politician'),
    signedOnlinePetition$pivotPlot[[8]]$data %>% mutate(quest='Signed a Petition Online'),
    participateVolunteerOrg$pivotPlot[[9]]$data %>% mutate(quest='Participated in Volunteer Org'),
    startedDiscussionPolitics$pivotPlot[[8]]$data %>% mutate(quest='Started Classroom Political Discussion'),
    purchasedBrandToSupportIssue$pivotPlot[[7]]$data %>% mutate(quest='Purchased Brand to Support Cause'),
    stoodUpToBullyForSomeone$pivotPlot[[4]]$data %>% mutate(quest='Stood Up to a Bully'),
    engagedCompanyToAdvocate$pivotPlot[[5]]$data %>% mutate(quest='Engaged Company to Advocate for Cause'),
    plannedSocialImpactActivity$pivotPlot[[8]]$data %>% mutate(quest='Planned an Activity for Social Impact'),
    donateTimeMoneyToCause$pivotPlot[[8]]$data %>% mutate(quest='Donated Time/Money to Cause')
  ) %>%
  group_by(quest) %>%
  mutate(meanVal = mean(avgVal)) %>%
  filter(race %in% c('Asian','Black','Hispanic/Latino','White','Multiracial'))

ggplot(whenActionsRace, aes(x=avgVal, y=reorder(quest, -meanVal), color=race)) +
  geom_point() +
  scale_x_continuous(
    labels=c('-1'='Unlikely To do','0'='Might Do','1'='Did Long Ago','2'='Done Past Year'),
    breaks=c(-1,0,1,2), limits=c(-1,2)
  ) +
  labs(y='',x='',title='') +
  scale_colour_brewer(palette = 'Set2',name='Race')

for (i in 1:length(beenPartOfCampaign$pivotPlot)) {
  print(paste(i,beenPartOfCampaign$pivotPlot[[i]]$labels$title))
}


whenActionsReligion <-
  bind_rows(
    takenActionEncouragedOnline$pivotPlot[[4]]$data %>% mutate(quest='Encouraged Others to Take Action Online'),
    contactedPolitician$pivotPlot[[4]]$data %>% mutate(quest='Contacted Politician'),
    signedOnlinePetition$pivotPlot[[4]]$data %>% mutate(quest='Signed a Petition Online'),
    participateVolunteerOrg$pivotPlot[[4]]$data %>% mutate(quest='Participated in Volunteer Org'),
    startedDiscussionPolitics$pivotPlot[[4]]$data %>% mutate(quest='Started Classroom Political Discussion'),
    leaderInClub$pivotPlot[[4]]$data %>% mutate(quest='Held Leadership Role in Club'),
    collectItemsForHomeless$pivotPlot[[3]]$data %>% mutate(quest='Collected Items for Homeless'),
    engagedCompanyToAdvocate$pivotPlot[[2]]$data %>% mutate(quest='Engaged Company to Advocate for Cause'),
    plannedSocialImpactActivity$pivotPlot[[4]]$data %>% mutate(quest='Planned an Activity for Social Impact'),
    donateTimeMoneyToCause$pivotPlot[[4]]$data %>% mutate(quest='Donated Time/Money to Cause'),
    startedCampaignToSolveProblem$pivotPlot[[2]]$data %>% mutate(quest='Started a Campaign'),
    beenPartOfCampaign$pivotPlot[[4]]$data %>% mutate(quest='Took Part in a Campaign')
  ) %>%
  group_by(quest) %>%
  mutate(meanVal = mean(avgVal))

ggplot(whenActionsReligion, aes(x=avgVal, y=reorder(quest, -meanVal), color=attend_religious_services_freq)) +
  geom_point() +
  scale_x_continuous(
    labels=c('-1'='Unlikely To do','0'='Might Do','1'='Did Long Ago','2'='Done Past Year'),
    breaks=c(-1,0,1,2), limits=c(-1,2)
  ) +
  labs(y='',x='',title='') +
  scale_colour_brewer(palette = 'RdBu',name='Religiosity')

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
  theme(plot.title = element_text(hjust=.5)) +
  labs(
    x='Age',y='Average Value',
    title='Signing an Online Petition')

p2 <-
  participateVolunteerOrg$pivotPlot[[10]] +
  geom_bar(aes(x=age,y=avgVal,fill='#6ac6b4'),stat='identity',alpha=.8) +
  geom_smooth(
    aes(x=age, y=avgVal),
    method='lm', se=F, color='#9e1f63', linetype='dashed', size=.75
  ) +
  theme(plot.title = element_text(hjust=.5)) +
  labs(
    x='Age',
    title='Participated in a Volunteer Organization'
    )

grid.arrange(
  p1,p2,ncol=2,
  top=textGrob("Comparison of Action Taken by Age",gp=gpar(fontsize=20)),
  bottom="Values range from -2 to 2 with -2 representing 'Would Never Do' and 2 representing 'Done Recently'"
  )

ggplot(issuesTakenAction$overall$data, aes(x=reorder(variable,-value),y=value)) +
  geom_bar(stat='identity', alpha=.8, fill='#6ac6b4') +
  geom_text(aes(label=percent(value)),vjust=-.5,size=2.8) +
  labs(
    title='Which Have You Taken Action on in the Past 12 months?',
    x=paste0('Average # Ticked = ',round(sum(issuesTakenAction$overall$data$value),2)),
    y='Percent Ticked'
    ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(hjust = .5)) +
  scale_y_continuous(breaks=pretty_breaks(10))

levs <-
  c('Very conservative: Avg # Ticked = 2.51', 'Conservative: Avg # Ticked = 2.7',
    'Moderate: Avg # Ticked = 3.3','Liberal: Avg # Ticked = 4.61',
    'Very Liberal: Avg # Ticked = 5.63')
ggplot(issuesTakenAction$pivotPlots$political_view$data, aes(x=reorder(variable, -value), y=value)) +
  geom_bar(stat='identity', alpha=.8, fill='#6ac6b4') +
  geom_text(aes(label=percent(value)),hjust=-.1,size=2) +
  facet_wrap(~factor(fac_labs,levels = levs), labeller = label_value, ncol=3) +
  labs(
    x='',
    title='',y='Percentage Ticked'
  ) +
  theme(plot.title = element_text(hjust = .5)) +
  scale_y_continuous(breaks=pretty_breaks(6),limits = c(0,.62)) +
  coord_flip()

ggplot(issuesTakenAction$pivotPlots$sex$data %>%
         filter(sex %in% c('Male','Female','Non-binary')),
       aes(x=reorder(variable, -value), y=value)) +
  geom_bar(stat='identity', alpha=.8, fill='#6ac6b4') +
  geom_text(aes(label=percent(value)),hjust=-.1,size=2.6) +
  facet_wrap(~fac_labs, labeller = label_value, ncol=3) +
  labs(
    x='',
    title='',y='Percentage Ticked'
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
    title='',y='Percentage Ticked'
  ) +
  theme(plot.title = element_text(hjust = .5)) +
  scale_y_continuous(breaks=pretty_breaks(6),limits = c(0,.5)) +
  coord_flip()

ggplot(issuesTakenAction$pivotPlots$attend_religious_services_freq$data %>%
         filter(attend_religious_services_freq %in% c('Multiple times a week','Never, agnostic or atheist')),
       aes(x=reorder(variable, -value), y=value)) +
  geom_bar(stat='identity', alpha=.8, fill='#6ac6b4') +
  geom_text(aes(label=percent(value)),hjust=-.1,size=2.3) +
  facet_wrap(~fac_labs, labeller = label_value, ncol=2) +
  labs(
    x='Which Issues Have You Taken Action On?',
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

planVoteIfEligible$groupedPivotPlot[[9]] +
  geom_text(aes(x=Group,y=avgVal,label=percent(avgVal)),vjust=-.1,size=2.8) +
  scale_y_continuous(breaks=pretty_breaks(10))

ggplot(voteWhenEligible$frequencyPlot$data,aes(x=outcome,y=count)) +
  geom_bar(stat='identity',fill='#6ac6b4',width = .8) +
  geom_text(aes(x=outcome,y=count,label=round(count)),vjust=-.1,size=3.4) +
  labs(title='Do You Plan to Vote When You Are Eligible', x='',y='') +
  scale_x_continuous(breaks=c(-1,0,1),labels = c('No','Undecided','Yes')) +
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
  labs(title='Prospects for the Future', x='',y='Percent Yes') +
  scale_y_continuous(breaks=pretty_breaks(10),limits = c(0,.55)) +
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
  scale_colour_brewer(palette = 'Set2') +
  scale_x_continuous(breaks=pretty_breaks(10)) +
  guides(colour=guide_legend(title="Political Views")) +
  theme(plot.title=element_text(hjust=.5))
