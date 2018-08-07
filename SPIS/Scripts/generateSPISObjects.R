
# Frequencies by Group ----------------------------------------------------

sexGroup <-
  stylePickOneList(set, sex, Group)

ageGroup <-
  stylePickOneList(set, age, Group)

raceGroup <-
  stylePickOneList(set, race, Group)

regionGroup <-
  stylePickOneList(set, region, Group)

stateGroup <-
  stylePickOneList(set, state, Group)

financesGroup <-
  stylePickOneList(set, fam_finances, Group)

parentEduGroup <-
  stylePickOneList(set, parental_education, Group)

partyGroup <-
  stylePickOneList(set, political_party, Group)

politicsGroup <-
  stylePickOneList(set, political_view, Group)

religiousGroup <-
  stylePickOneList(set, attend_religious_services_freq, Group)

# Impact Attitudes -----------------------------------------------------------------

mapFrom <- c('Strongly Disagree','2','3','4','Strongly Agree')
mapTo <- c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree')
finCode <- c(-2,-1,0,1,2)
effortUnderstandPerspectives <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_make_an_active_effort_to_understand_others_perspectives,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

partOfSocialMovement <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_am_part_of_a_larger_social_movement_to_solve_social_problems,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

iCanSolveProblems <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_can_solve_social_problems,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

myParticipationMatters <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.My_participation_in_local_issues_matters,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

comfortAssertingMyself <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_am_comfortable_asserting_myself_e_g__to_advocate_for_an_issue_with_peers_or_within_my_school_community_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

iHaveConfidence <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_have_confidence_in_myself,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

iCanCollaborate <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_can_collaborate_effectively_with_others_e_g__work_with_others_on_a_volunteer_project_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

exposedOtherOpinions <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_am_often_exposed_to_other_opinions_and_world_views_I_don_t_necessarily_agree_with,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

iTakeActionsIssues <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_often_take_action_on_social_issues_that_I_learn_about,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

# Which Action Taken When -------------------------------------------------

mapFrom <-
  c(
    'Not done and would NEVER under any circumstances',
    'Not done and DONT KNOW if I would in the future',
    'Not done but MIGHT in the future',
    'Done over a year ago',
    'Done in the past year'
    )
mapTo <-
  c(
    'Would Never Do',
    'Not Done, Dont Know if Ever',
    'Not Done But Might',
    'Done in Past',
    'Done Past Year'
    )
finCode <- c(-2,-1,0,1,2)

politicalEventsProtests <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Attended_a_political_event_or_participated_in_a_protest_of_any_kind_e_g_campaign_event_town_hall_organized_protest_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

takenActionEncouragedOnline <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Encouraged_other_people_to_take_action_on_a_political_or_social_issue_that_is_important_to_you_ONLINE,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

contactedPolitician <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Contacted_an_elected_official_about_an_issue_or_concern,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

signedOnlinePetition <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Signed_a_petition_about_a_political_or_social_issue_ONLINE,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

participateVolunteerOrg <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Participated_in_a_political_charitable_or_religious_based_volunteer_organization,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

startedDiscussionPolitics <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Started_a_discussion_conversation_in_class_on_a_social_or_political_issue,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

purchasedBrandToSupportIssue <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Purchased_a_brand_product_because_you_wanted_to_show_support_for_the_issues_they_stood_up_for_represented_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

boycottedBrandForIssue <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Stopped_purchasing_boycotted_a_brand_or_company_because_they_stood_for_something_or_behaved_in_a_way_that_didn_t_align_with_your_values_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

stoodUpToBullyForSomeone <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Helped_a_stranger_or_peer_in_real_life_who_needed_help_e_g__stood_up_for_a_peer_against_a_bully_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

leaderInClub <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Held_a_leadership_role_e_g_of_a_school_club_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

collectItemsForHomeless <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Collected_items_for_a_social_purpose_e_g__collected_clothes_for_youth_young_people_experiencing_homelessness_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

researchSocialIssueOutsideSchool <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Researched_current_or_controversial_issues_outside_of_class_time_or_requirements,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

engagedCompanyToAdvocate <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Engaged_with_a_company_to_advocate_for_a_social_cause_e_g__tweeted_at_a_makeup_company_urging_them_to_have_cruelty_free_products_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

plannedSocialImpactActivity <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Planned_social_impact_related_activities_or_events_for_a_group_team_or_club_e_g__planned_a_fundraiser_for_my_your_school_club_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

donateTimeMoneyToCause <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Given_time_money_or_other_donations_to_help_a_social_cause_e_g__donated_food_to_a_local_food_bank_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

createdPetition <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Created_a_petition_around_a_social_issue_you_identified_e_g__created_a_petition_to_support_recycling_after_finding_out_your_school_doesn_t_recycle_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

startedCampaignToSolveProblem <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Started_a_campaign_to_solve_a_social_problem_e_g__started_a_campaign_for_safe_spaces_at_school_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

beenPartOfCampaign <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Been_part_of_a_collective_social_change_campaign,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

# Since Enagaging with DS -------------------------------------------------

mapFrom <- c('No','Yes')
mapTo <- c('No','Yes')
finCode <- c(0,1)

sinceDS.considerEffectActionsOnOthers <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Am_more_likely_to_take_into_consideration_the_effect_of_my_actions_on_others_my_community_and_the_environment,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.amPartOfSocialMovement <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Am_part_of_a_larger_social_movement_to_solve_social_problems,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.iCanSolveProblems <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Believe_I_can_solve_social_problems,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.haveToolsToTakeAction <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Have_the_tools_and_resources_I_need_to_take_action_on_social_issues,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.easyToImpactSociety <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Believe_it_is_easy_to_make_a_positive_impact_on_society,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.iHaveConfidence <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Have_more_confidence_in_myself,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.persistThroughChallenges <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Have_learned_to_persist_through_challenges,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.iCanAccomplishGoals <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Believe_I_can_accomplish_my_goals,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.learnedCollaborateOthers <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Have_learned_how_to_collaborate_effectively_with_others,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.learnedAnalyzeSolveProblems <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Have_learned_how_to_analyze_problems_and_implement_creative_solutions,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.effortUnderstandPerspectives <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.I_make_more_of_an_effort_to_understand_others_perspectives,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.thinkLocalIssuesMatter <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.I_feel_my_participation_in_local_issues_matters,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.passionateNewCause <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Discovered_a_social_cause_I_am_passionate_about,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.startedSocialJusticeOrg <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Started_my_own_social_justice_organization_or_club,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.learnedOrganizeOthers <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Have_learned_how_to_organize_others_e_g_unite_a_group_of_peers_in_a_school_club_to_work_on_a_social_cause_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.PickSchoolCourses <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    school_courses_bc_DS,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.continuedActivityAfterCampaign <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    continued_DS_activities_after_campaign,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.NoneOfThese <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.None_of_the_above,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

# Issues Taken Action on 12mo ---------------------------------------------

issuesTakenAction <-
  styleSelectMultiple(
    set,
    'which_issues_taken_action_12mo.',
    pivots=c(Group, sex, fam_finances, age, race, region, parental_education,
             political_party, political_view, attend_religious_services_freq,
             grade_level, signups, reportbacks)
  )

# Volunteering ------------------------------------------------------------

mapFrom <- c('Never','One time only','Once every few months',
             'About once a month','2-3 times a month','About once a week',
             'More than once a week')
mapTo <- c('Never','One time only','Once every few months',
           'About once a month','2-3 times a month','About once a week',
           'More than once a week')
finCode <- c(1,2,3,4,5,6,7)
volunteerFrequency <-
  stylePickOneOrdinal(
    set,
    volunteer_freq,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

mapFrom <- c('No','Yes')
mapTo <- c('No','Yes')
finCode <- c(0,1)
volunteerInformal <-
  stylePickOneOrdinal(
    set,
    volunteer_informal,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

volunteerReason <-
  styleSelectMultiple(
    set,
    'volunteer_reason.',
    pivots=c(Group, sex, fam_finances, age, race, region, parental_education,
             political_party, political_view, attend_religious_services_freq,
             grade_level, signups, reportbacks)
  )

# Voting ------------------------------------------------------------------

mapFrom <- c('No','Undecided','Yes')
mapTo <- c('No','Undecided','Yes')
finCode <- c(-1,0,1)
planVoteIfEligible <-
  stylePickOneOrdinal(
    set %>% filter(plan_to_vote_if_eligible %in% c('No','Yes','Undecided')),
    plan_to_vote_if_eligible,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

voteWhenEligible <-
  stylePickOneOrdinal(
    set %>% filter(!is.na(vote_when_eligible)),
    vote_when_eligible,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

# Top Issues --------------------------------------------------------------

mapFrom <- c(NA,'1','2','3','4','5')
mapTo <- c('Not Top 5','1st','2nd','3rd','4th','5th')
finCode <- c(6,1,2,3,4,5)

topIssues.Terrorism <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Defending_the_country_from_future_terrorist_attacks,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.Education <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Improving_the_educational_system_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.RacialJustice <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Dealing_with_racial_justice,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.Immigration <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Dealing_with_the_issue_of_immigration_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.ClimateChange <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Dealing_with_global_climate_change_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.GenderEquality <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Dealing_with_gender_equality,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.Crime <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Reducing_crime,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.Poverty <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Dealing_with_the_problems_of_poor_and_needy_people,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.IncomeInequality <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Reducing_income_inequality,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.Jobs <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Improving_the_job_situation,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.Military <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Strengthening_the_U_S_military,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.CountryDebt <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Reducing_the_country_s_debt,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.AccessHealthcare <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Improving_access_to_mental_health_care,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.CostsHealthcare <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Reducing_health_care_costs,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.Economy <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Strengthening_the_nation_s_economy,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.CollegeAffordability <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Taking_steps_to_make_a_college_education_affordable_to_all,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.TaxPolicy <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Reforming_the_nation_s_tax_system,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.CriminalJusticeReform <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Reforming_the_criminal_justice_system,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.GunPolicy <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Dealing_with_gun_policy,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.Environment <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Protecting_the_environment,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.ReligiousIntolerance <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Decreasing_religious_intolerance,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

topIssues.DrugOverdoses <-
  stylePickOneOrdinal(
    set,
    top_issues_prompted.Addressing_the_drug_overdose_crisis,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

# Agree With Position -----------------------------------------------------

mapFrom <- c('Strongly Disagree','Disagree','Uncertain',
             'Unfamiliar with this topic','Agree','Strongly Agree')
mapTo <- c('Strongly Disagree','Disagree','Neutral',
           'Neutral','Agree','Strongly Agree')
finCode <- c(-2,-1,0,0,1,2)

agreePosition.RacismNotIssue <-
  stylePickOneOrdinal(
    set,
    agree_positions.Racism_is_no_longer_an_issue_in_America_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

agreePosition.GovernmentProvideHealthcare <-
  stylePickOneOrdinal(
    set,
    agree_positions.The_government_has_the_responsibility_to_ensure_health_coverage_for_all,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

agreePosition.UndocumentedImmigrantsPathCitizenship <-
  stylePickOneOrdinal(
    set,
    agree_positions.America_should_offer_undocumented_immigrants_a_path_to_citizenship_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

agreePosition.BackgroundChecksForGuns <-
  stylePickOneOrdinal(
    set,
    agree_positions.Background_checks_should_be_required_for_everyone_who_buys_a_gun_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

agreePosition.SexualHarrasmentSignOffSocietalIssue <-
  stylePickOneOrdinal(
    set,
    agree_positions.Allegations_of_sexual_harassment_and_assault_mainly_reflect_widespread_problems_in_society,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

agreePosition.iTrustPoliceMyCommunity <-
  stylePickOneOrdinal(
    set,
    agree_positions.I_trust_the_police_in_my_community_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

agreePosition.WeedLawsTooStrict <-
  stylePickOneOrdinal(
    set,
    agree_positions.Marijuana_laws_are_too_strict_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

agreePosition.ClimateChangeHappening <-
  stylePickOneOrdinal(
    set,
    agree_positions.Climate_change_is_happening_and_is_caused_by_human_activity_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

agreePosition.BuildTheWall <-
  stylePickOneOrdinal(
    set,
    agree_positions.America_should_build_a_wall_on_the_Mexican_border_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

agreePosition.OnlyTwoGenders <-
  stylePickOneOrdinal(
    set,
    agree_positions.There_are_only_two_genders_male_and_female_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

agreePosition.WomenExperienceWorkplaceDiscrimination <-
  stylePickOneOrdinal(
    set,
    agree_positions.Women_experience_barriers_that_prevent_them_from_advancing_in_their_workplace_even_if_they_are_qualified,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

agreePosition.MuslimsDontNeedCloseTabs <-
  stylePickOneOrdinal(
    set,
    agree_positions.Muslims_living_in_the_US_should_not_be_subject_to_additional_scrutiny_solely_because_of_their_religion,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

agreePosition.DrugsProblemInCommunity <-
  stylePickOneOrdinal(
    set,
    agree_positions.Drug_addiction_is_a_problem_in_my_community,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

# Cause Importance --------------------------------------------------------

mapFrom <- c('Not at all important','2','3','4','Very Important')
mapTo <- c('Not Important','A Little','Neutral','Important','Very Important')
finCode <- c(-2,-1,0,1,2)

causeImportance.RacialEquality <-
  stylePickOneOrdinal(
    set,
    causes_importance.Racial_equality,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

causeImportance.IncomeInequality <-
  stylePickOneOrdinal(
    set,
    causes_importance.Income_inequality_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

causeImportance.Immigration <-
  stylePickOneOrdinal(
    set,
    causes_importance.Immigration_policy,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

causeImportance.GenderEquality <-
  stylePickOneOrdinal(
    set,
    causes_importance.Gender_equality,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

causeImportance.Environment <-
  stylePickOneOrdinal(
    set,
    causes_importance.The_environment,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

causeImportance.LGBTQRight <-
  stylePickOneOrdinal(
    set,
    causes_importance.LGBTQ_rights,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

causeImportance.SexualHarrassmentAssault <-
  stylePickOneOrdinal(
    set,
    causes_importance.Sexual_harassment_and_assault,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

causeImportance.AnimalWelfare <-
  stylePickOneOrdinal(
    set,
    causes_importance.Animal_welfare,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

causeImportance.DisasterRelief <-
  stylePickOneOrdinal(
    set,
    causes_importance.Disaster_relief,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

causeImportance.Bullying <-
  stylePickOneOrdinal(
    set,
    causes_importance.Bullying,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

causeImportance.Mental_Health <-
  stylePickOneOrdinal(
    set,
    causes_importance.Mental_Health,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

causeImportance.PhysicalHealth <-
  stylePickOneOrdinal(
    set,
    causes_importance.Physical_Health,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

causeImportance.HomelessnessPoverty <-
  stylePickOneOrdinal(
    set,
    causes_importance.Homelessness_and_or_poverty,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

causeImportance.GunViolence <-
  stylePickOneOrdinal(
    set,
    causes_importance.Gun_violence_prevention,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

causeImportance.VoterReg <-
  stylePickOneOrdinal(
    set,
    causes_importance.Voter_registration,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

# Student March -----------------------------------------------------------

mapFrom <- c('Not very likely','Not likely','Neutral',
             'Dont know it would depend on the issue','Likely','Very likely','Very Likely')
mapTo <- c('Not Very Likely','Not Likely','Neutral','Neutral','Likely',
           'Very Likely','Very Likely')
finCode <- c(-2,-1,0,0,1,2,2)

howLikelyStudentMarch <-
  stylePickOneOrdinal(
    set,
    how_likely_student_march,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

# Brand -----------------------------------------------

mapFrom <- c('Not at all influential','2','3','4','Very influential')
mapTo <- c('Not Influential','A Little','Neutral','Influential','Very Influential')
finCode <- c(-2,-1,0,1,2)

purchaseInfluence.Price <-
  stylePickOneOrdinal(
    set,
    influence_when_purchasing.Price,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

purchaseInfluence.Quality <-
  stylePickOneOrdinal(
    set,
    influence_when_purchasing.Quality_of_product_service,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

purchaseInfluence.Packaging <-
  stylePickOneOrdinal(
    set,
    influence_when_purchasing.Packaging,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

purchaseInfluence.Celebrity <-
  stylePickOneOrdinal(
    set,
    influence_when_purchasing.Celebrity_Influencer_endorsement,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

purchaseInfluence.CompanyReputationValues <-
  stylePickOneOrdinal(
    set,
    influence_when_purchasing.Values_reputation_of_the_brand_or_company_e_g_treats_employees_fairly_gives_back_to_society_socially_or_environmentally_responsible_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

purchaseInfluence.RecommendedByKnownPerson <-
  stylePickOneOrdinal(
    set,
    influence_when_purchasing.Recommended_by_people_you_know_e_g_friends_family_co_workers_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

purchaseInfluence.CustomerReviews <-
  stylePickOneOrdinal(
    set,
    influence_when_purchasing.Customer_reviews,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

mapFrom <- c('All the time','Never','Often','Rarely','Sometimes')
mapTo <- c('All the time','Never','Often','Rarely','Sometimes')
finCode <- c(4,0,3,1,2)

FrequencyPurchaseBrandValues <-
  stylePickOneOrdinal(
    set %>% filter(purchasing_due_to_brand_values != 'Not sure'),
    purchasing_due_to_brand_values,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

mapFrom <- c('No','Not Sure','Yes')
mapTo <- c('No','Not Sure','Yes')
finCode <- c(-1,0,1)

brandValuesCompensate.LowerQuality <-
  stylePickOneOrdinal(
    set %>% filter(purchasing_due_to_brand_values != 'Not sure'),
    what_compensates_for_good_brand_values.It_was_lower_in_quality_than_a_competitor,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

brandValuesCompensate.HigherPrice <-
  stylePickOneOrdinal(
    set %>% filter(purchasing_due_to_brand_values != 'Not sure'),
    what_compensates_for_good_brand_values.It_was_more_expensive_than_a_competitor,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

brandValuesCompensate.DidntKnowAboutBrand <-
  stylePickOneOrdinal(
    set %>% filter(purchasing_due_to_brand_values != 'Not sure'),
    what_compensates_for_good_brand_values.You_or_no_one_you_knew_had_ever_heard_of_it,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

mapFrom <- c('Dont know',NA,'Less than 10','10-20','More than 20')
mapTo <- c('Uncertain','Uncertain','Less than 10%','10-20%','More than 20%')
finCode <- c(0,0,1,2,3)
willingPayMoreBrandValues <-
  stylePickOneOrdinal(
    dat=set %>% filter(!is.na(willing_to_pay_how_much_more_brand_good_values)),
    willing_to_pay_how_much_more_brand_good_values,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

mapFrom <- c('Very untrue of me','2','3','4','Very true of me')
mapTo <- c('Very Untrue','Somewhat Untrue','Uncertain','Somewhat True','Very True')
finCode <- c(-2,-1,0,1,2)

myPurchaseSupportCauseMakesImpact <-
  stylePickOneOrdinal(
    dat=set,
    purchases_impactful_seek_good_brands.I_feel_like_my_purchases_are_making_an_impact_when_the_brand_I_buy_from_supports_a_cause_I_believe_in,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

iSeekResponsibleBrands <-
  stylePickOneOrdinal(
    dat=set,
    purchases_impactful_seek_good_brands.I_actively_seek_out_socially_or_environmentally_responsible_brands_to_shop_from_rather_than_passively_stumble_upon_them,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

mapFrom <- c('Not at all important','Not very important','Neutral',
             'Somewhat important','Very Important')
mapTo <- c('Not Important','A Little Important','Uncertain',
           'Somewhat Important','Very Important')
finCode <- c(-2,-1,0,1,2)
howImportantBrandHaveSocialChangeInitiative <-
  stylePickOneOrdinal(
    dat=set %>% filter(important_brand_be_good!='Dont know or unfamiliar with social change initiatives that consumers can be a part of'),
    important_brand_be_good,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

# Products Used Past 12mo -------------------------------------------------

productsUsed <-
  styleSelectMultiple(
    set,
    'products_used_past12mo.',
    pivots=c(Group, sex, fam_finances, age, race, region, parental_education,
             political_party, political_view, attend_religious_services_freq,
             grade_level, signups, reportbacks)
  )

#TODO: Get counts of using and then show proportion who would delete/keep
whichAppDeleteGroup <-
  stylePickOneList(set, XTRwhich_delete_first, Group)

whichAppKeepGroup <-
  stylePickOneList(set, XTRwhich_keep_most, Group)

# Which Will Occur --------------------------------------------------------

mapFrom <- c(0,1)
mapTo <- c(0,1)
finCode <- c(0,1)

willOccur.iRunForPublicOffice <-
  stylePickOneOrdinal(
    dat=set,
    which_will_occur.Run_for_public_office,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

willOccur.iWillLiveLong <-
  stylePickOneOrdinal(
    dat=set,
    which_will_occur.Live_abroad_for_an_extended_period_of_time,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

willOccur.iWillChangeTheWorld <-
  stylePickOneOrdinal(
    dat=set,
    which_will_occur.Change_the_world_in_a_meaningful_way,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

willOccur.iWillGetRich <-
  stylePickOneOrdinal(
    dat=set,
    which_will_occur.Become_rich,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

willOccur.iWillHaveBetterLifeThanParents <-
  stylePickOneOrdinal(
    dat=set,
    which_will_occur.Have_a_better_life_than_my_parents,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

# Non Profit Awareness ----------------------------------------------------

mapFrom <- c('Not at all familiar','2','3','4','Very familiar')
mapTo <- c('Not Familiar','A little','Neutral','Somewhat','Very Familiar')
finCode <- c(-2,-1,0,1,2)

nonprofitAwareness.DoSomething <-
  stylePickOneOrdinal(
    dat=set %>% filter(Group=='Gen Pop'),
    non_profit_awareness.DoSomething_org,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

nonprofitAwareness.WeDotOrg <-
  stylePickOneOrdinal(
    dat=set %>% filter(Group=='Gen Pop'),
    non_profit_awareness.WE_org,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

nonprofitAwareness.KeyClub <-
  stylePickOneOrdinal(
    dat=set %>% filter(Group=='Gen Pop'),
    non_profit_awareness.Key_Club_International,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

nonprofitAwareness.4H <-
  stylePickOneOrdinal(
    dat=set %>% filter(Group=='Gen Pop'),
    non_profit_awareness._H,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

nonprofitAwareness.ChangeDotOrg <-
  stylePickOneOrdinal(
    dat=set %>% filter(Group=='Gen Pop'),
    non_profit_awareness.Change_org,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

nonprofitAwareness.DonorsChoose <-
  stylePickOneOrdinal(
    dat=set %>% filter(Group=='Gen Pop'),
    non_profit_awareness.DonorsChoose_org,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level, signups, reportbacks),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

whereSeeDoSomething <-
  styleSelectMultiple(
    set %>% filter(Group=='Gen Pop'),
    'where_see_DS.',
    pivots=c(Group, sex, fam_finances, age, race, region, parental_education,
             political_party, political_view, attend_religious_services_freq,
             grade_level, signups, reportbacks)
  )

socialMediaSeeDoSomething <-
  styleSelectMultiple(
    set %>% filter(Group=='Gen Pop'),
    'which_social_media_see_DS.',
    pivots=c(Group, sex, fam_finances, age, race, region, parental_education,
             political_party, political_view, attend_religious_services_freq,
             grade_level, signups, reportbacks)
  )

howEngageDoSomething <-
  styleSelectMultiple(
    set %>% filter(Group=='Gen Pop'),
    'how_engage_with_DS.',
    pivots=c(Group, sex, fam_finances, age, race, region, parental_education,
             political_party, political_view, attend_religious_services_freq,
             grade_level, signups, reportbacks)
  )

# NPS ---------------------------------------------------------------------

npsBreakdown <- getNPSBreakdown(set %>% filter(!is.na(nps)), 'nps')
