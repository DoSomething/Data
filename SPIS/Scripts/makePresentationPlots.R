
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

purchaseInfluence.Price$pivotPlot[[1]]

# Social Action Types by Age ----------------------------------------------

p1 <-
  signedOnlinePetition$pivotPlot[[7]] +
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
  participateVolunteerOrg$pivotPlot[[9]] +
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
