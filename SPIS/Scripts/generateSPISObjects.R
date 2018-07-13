
# Frequencies by Group ----------------------------------------------------

stateGroup <-
  stylePickOneList(set, state, Group)

sexGroup <-
  stylePickOneList(set, sex, Group)

financesGroup <-
  stylePickOneList(set, fam_finances, Group)

parentEduGroup <-
  stylePickOneList(set, parental_education, Group)

religiousGroup <-
  stylePickOneList(set, attend_religious_services_freq, Group)

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
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

partOfSocialMovement <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_am_part_of_a_larger_social_movement_to_solve_social_problems,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

iCanSolveProblems <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_can_solve_social_problems,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

myParticipationMatters <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.My_participation_in_local_issues_matters,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

comfortAssertingMyself <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_am_comfortable_asserting_myself_e_g__to_advocate_for_an_issue_with_peers_or_within_my_school_community_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

iHaveConfidence <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_have_confidence_in_myself,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

iCanCollaborate <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_can_collaborate_effectively_with_others_e_g__work_with_others_on_a_volunteer_project_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

exposedOtherOpinions <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_am_often_exposed_to_other_opinions_and_world_views_I_don_t_necessarily_agree_with,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

iTakeActionsIssues <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_often_take_action_on_social_issues_that_I_learn_about,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

# Which Action Taken When -------------------------------------------------

mapFrom <-
  c('Done in the past year',
    'Done over a year ago',
    'Not done and DONT KNOW if I would in the future',
    'Not done and would NEVER under any circumstances',
    'Not done but MIGHT in the future'
    )
mapTo <- c('Done Past Year','Done in Past',
           'Not Done, Dont Know if Ever','Would Never Do',
           'Not Done But Might')
finCode <- c(2,1,0,-2,-1)

politicalEventsProtests <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Attended_a_political_event_or_participated_in_a_protest_of_any_kind_e_g_campaign_event_town_hall_organized_protest_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

takenActionEncouragedOnline <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Encouraged_other_people_to_take_action_on_a_political_or_social_issue_that_is_important_to_you_ONLINE,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

contactedPolitician <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Contacted_an_elected_official_about_an_issue_or_concern,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

signedOnlinePetition <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Signed_a_petition_about_a_political_or_social_issue_ONLINE,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

participateVolunteerOrg <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Participated_in_a_political_charitable_or_religious_based_volunteer_organization,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

startedDiscussionPolitics <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Started_a_discussion_conversation_in_class_on_a_social_or_political_issue,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

purchasedBrandToSupportIssue <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Purchased_a_brand_product_because_you_wanted_to_show_support_for_the_issues_they_stood_up_for_represented_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

boycottedBrandForIssue <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Stopped_purchasing_boycotted_a_brand_or_company_because_they_stood_for_something_or_behaved_in_a_way_that_didn_t_align_with_your_values_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

stoodUpToBullyForSomeone <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Helped_a_stranger_or_peer_in_real_life_who_needed_help_e_g__stood_up_for_a_peer_against_a_bully_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

leaderInClub <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Held_a_leadership_role_e_g_of_a_school_club_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

collectItemsForHomeless <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Collected_items_for_a_social_purpose_e_g__collected_clothes_for_youth_young_people_experiencing_homelessness_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

researchSocialIssueOutsideSchool <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Researched_current_or_controversial_issues_outside_of_class_time_or_requirements,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

engagedCompanyToAdvocate <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Engaged_with_a_company_to_advocate_for_a_social_cause_e_g__tweeted_at_a_makeup_company_urging_them_to_have_cruelty_free_products_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

plannedSocialImpactActivity <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Planned_social_impact_related_activities_or_events_for_a_group_team_or_club_e_g__planned_a_fundraiser_for_my_your_school_club_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

donateTimeMoneyToCause <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Given_time_money_or_other_donations_to_help_a_social_cause_e_g__donated_food_to_a_local_food_bank_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

createdPetition <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Created_a_petition_around_a_social_issue_you_identified_e_g__created_a_petition_to_support_recycling_after_finding_out_your_school_doesn_t_recycle_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

startedCampaignToSolveProblem <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Started_a_campaign_to_solve_a_social_problem_e_g__started_a_campaign_for_safe_spaces_at_school_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

beenPartOfCampaign <-
  stylePickOneOrdinal(
    set,
    which_actions_taken_and_when.Been_part_of_a_collective_social_change_campaign,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
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
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.amPartOfSocialMovement <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Am_part_of_a_larger_social_movement_to_solve_social_problems,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.iCanSolveProblems <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Believe_I_can_solve_social_problems,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.haveToolsToTakeAction <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Have_the_tools_and_resources_I_need_to_take_action_on_social_issues,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.easyToImpactSociety <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Believe_it_is_easy_to_make_a_positive_impact_on_society,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.iHaveConfidence <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Have_more_confidence_in_myself,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.persistThroughChallenges <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Have_learned_to_persist_through_challenges,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.iCanAccomplishGoals <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Believe_I_can_accomplish_my_goals,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.learnedCollaborateOthers <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Have_learned_how_to_collaborate_effectively_with_others,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.learnedAnalyzeSolveProblems <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Have_learned_how_to_analyze_problems_and_implement_creative_solutions,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.effortUnderstandPerspectives <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.I_make_more_of_an_effort_to_understand_others_perspectives,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.thinkLocalIssuesMatter <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.I_feel_my_participation_in_local_issues_matters,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.passionateNewCause <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Discovered_a_social_cause_I_am_passionate_about,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.startedSocialJusticeOrg <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Started_my_own_social_justice_organization_or_club,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.learnedOrganizeOthers <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.Have_learned_how_to_organize_others_e_g_unite_a_group_of_peers_in_a_school_club_to_work_on_a_social_cause_,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.PickSchoolCourses <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    school_courses_bc_DS,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

sinceDS.NoneOfThese <-
  stylePickOneOrdinal(
    set %>% filter(Group=='Members'),
    since_engage_DS.None_of_the_above,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )
