set %>%
  filter(Group=='Gen Pop' &
           agree_positions.Background_checks_should_be_required_for_everyone_who_buys_a_gun_ != 'Unfamiliar with this topic') %>%
  count(agree_positions.Background_checks_should_be_required_for_everyone_who_buys_a_gun_) %>%
  mutate(p=n/sum(n))

set %>%
  filter(Group=='Gen Pop' &
           agree_positions.Background_checks_should_be_required_for_everyone_who_buys_a_gun_ != 'Unfamiliar with this topic') %>%
  group_by(agree_positions.Background_checks_should_be_required_for_everyone_who_buys_a_gun_) %>%
  summarise(n=sum(weight)) %>%
  mutate(p=n/sum(n))

set %>%
  filter(Group=='Gen Pop' &
           agree_positions.The_government_has_the_responsibility_to_ensure_health_coverage_for_all != 'Unfamiliar with this topic') %>%
  count(agree_positions.The_government_has_the_responsibility_to_ensure_health_coverage_for_all) %>%
  mutate(p=n/sum(n))

set %>%
  filter(Group=='Gen Pop' &
           agree_positions.The_government_has_the_responsibility_to_ensure_health_coverage_for_all != 'Unfamiliar with this topic') %>%
  group_by(agree_positions.The_government_has_the_responsibility_to_ensure_health_coverage_for_all) %>%
  summarise(n=sum(weight)) %>%
  mutate(p=n/sum(n))

set %>%
  filter(Group=='Gen Pop' &
           agree_positions.America_should_offer_undocumented_immigrants_a_path_to_citizenship_ != 'Unfamiliar with this topic') %>%
  count(agree_positions.America_should_offer_undocumented_immigrants_a_path_to_citizenship_) %>%
  mutate(p=n/sum(n))

set %>%
  filter(Group=='Gen Pop' &
           agree_positions.America_should_offer_undocumented_immigrants_a_path_to_citizenship_ != 'Unfamiliar with this topic') %>%
  group_by(agree_positions.America_should_offer_undocumented_immigrants_a_path_to_citizenship_) %>%
  summarise(n=sum(weight)) %>%
  mutate(p=n/sum(n))

set %>%
  filter(Group=='Gen Pop' &
           agree_positions.Climate_change_is_happening_and_is_caused_by_human_activity_ != 'Unfamiliar with this topic') %>%
  count(agree_positions.Climate_change_is_happening_and_is_caused_by_human_activity_) %>%
  mutate(p=n/sum(n))

set %>%
  filter(Group=='Gen Pop' &
           agree_positions.Climate_change_is_happening_and_is_caused_by_human_activity_ != 'Unfamiliar with this topic') %>%
  group_by(agree_positions.Climate_change_is_happening_and_is_caused_by_human_activity_) %>%
  summarise(n=sum(weight)) %>%
  mutate(p=n/sum(n))

set %>%
  group_by(Group,causes_importance.Voter_registration) %>%
  summarise(sum(weight))