* Encoding: UTF-8.
* Recode NPS score from 1-11 point scale in QuestionPro to standard 0-10 point scale labels.

Compute NPS_rec=NPS.
If (NPS=1) NPS_rec=0.
If (NPS=2) NPS_rec=1.
If (NPS=3) NPS_rec=2.
If (NPS=4) NPS_rec=3.
If (NPS=5) NPS_rec=4.
If (NPS=6) NPS_rec=5.
If (NPS=7) NPS_rec=6.
If (NPS=8) NPS_rec=7.
If (NPS=9) NPS_rec=8.
If (NPS=10) NPS_rec=9.
If (NPS=11) NPS_rec=10.
execute.

*Check recoded correctly.
crosstabs NPS by NPS_rec.
execute.

FREQUENCIES variables=NPS NPS_rec.
execute.

*Add value labels to brand perception.
Value labels
Value
1 "Scholarship opportunities"
2 "Fun and original ways to make a difference"
3 "An opportunity to feel part of something larger"
4 "Resources that make volunteering easy"
5 "Other".
execute.

*Add value labels to age.
Value labels
Age
1 "12 or younger"
2 "13"
3 "14"
4 "15"
5 "16"
6 "17"
7 "18"
8 "19"
9 "20"
10 "21"
11 "22"
12 "23"
13 "24"
14 "25"
15 "26 or older".
execute.

Compute Age_rec=Age.
If (age=1) age_rec=12.
If (age=2) age_rec=13.
If (age=3) age_rec=14.
If (age=4) age_rec=15.
If (age=5) age_rec=16.
If (age=6) age_rec=17.
If (age=7) age_rec=18.
If (age=8) age_rec=19.
If (age=9) age_rec=20.
If (age=10) age_rec=21.
If (age=11) age_rec=22.
If (age=12) age_rec=23.
If (age=13) age_rec=24.
If (age=14) age_rec=25.
If (age=15) age_rec=26.
execute.

*Create NPS.
Compute NPS_cat = NPS_rec.
If (NPS_rec<7) NPS_cat = 1.
If ((NPS_rec>=7) and (NPS_rec<9)) NPS_Cat = 2.
If (NPS_rec>=9) NPS_cat =3.
execute.

Value labels
NPS_cat
1 "Detractors"
2 "Persuadables"
3 "Promoters".
execute.

FREQUENCIES VARIABLES=NPS NPS_cat
  /ORDER=ANALYSIS.

*check recoded NPS categories correctly..
crosstabs NPS by NPS_cat.
execute.

*Add value labels.
Value labels
DS_interact
1 "I sign up for a campaign/project to learn more information but don't do the actions DS suggests"
2 "I sign up for a campaign/project and do the actions DS suggests, but don't always upload or text photos to DS"
3 "I sign up for a campaign/project, do the actions DS suggests, and upload or text photos to DS whenever I interact with DS"
4 "I don't use DS".
execute.

Value labels
Participation
1 "In the last week"
2 "In the last month"
3 "2-6 months ago"
4 "Over 6 months ago"
5 "I have never participated in a DS campaign/project".
execute.

Value labels
Another_campaign
1 "Completely Likely"
2 "Very likely"
3 "Slightly Likely"
4 "Not at all Likely"
5 "Don't know".
execute.

*Create dummy variables for NPS categories.
Compute persuadable=0.
If (NPS_cat=2) persuadable=1.
execute.

crosstabs persuadable by  NPS_cat.
execute.

Compute promoter=0.
If (NPS_cat=3) promoter=1.
execute.

crosstabs promoter by  NPS_cat.
execute.

Compute detractor=0.
If (NPS_cat=1) detractor=1.
execute.

crosstabs detractor by  NPS_cat.
execute.

*Create collapsed variable for participation.
Compute Participation_rec=Participation.
If (Participation<3) Participation_rec=1.
If (Participation=3) Participation_rec=2.
If (Participation=4) Participation_rec=3.
If (Participation=5) Participation_rec=4.
execute.

crosstabs Participation by Participation_rec.
execute.

*Create collapsed variable for re-engagement in next month.
Compute Another_campaign_rec=Another_campaign.
If (Another_campaign<3) Another_campaign_rec=1.
If (Another_campaign=3) Another_campaign_rec=2.
If (Another_campaign=4) Another_campaign_rec=3.
If (Another_campaign=5) Another_campaign_rec=4.
execute.

crosstabs Another_campaign by Another_campaign_rec.
execute.

*Create new variable for number of signups.
Compute signups=count_of_signups.
If (count_of_signups>5) signups=6.
execute.

Crosstabs count_of_signups by signups.
execute.

*Create dummy variables for all touchpoints.
Compute causes_five=0.
If (Causes=5) causes_five=1.
execute.

crosstabs Causes by causes_five.
execute.

FREQUENCIES VARIABLES=Causes causes_five 
  /ORDER=ANALYSIS.

Compute communication_rec=0.
If (Communication=5) communication_rec=1.
execute.

crosstabs communication by communication_rec.
execute.

Compute website_five=0.
If (website=5) website_five=1.
execute.

Compute campaigns_five=0.
If (campaigns=5) campaigns_five=1.
execute.

crosstabs campaigns by campaigns_five.
execute.

Compute scholarship_five=0.
If(scholarships=5) scholarship_five=1.
execute.

crosstabs scholarships by scholarships_five.
execute.


Compute community_five=0.
If (community=5) Community_five=1.
execute.

crosstabs community by community_five.
execute.

*Create dummy variable for re-engagement.
Compute reengage=0.
If (Another_campaign_rec=1) reengage=1.
execute.

Crosstabs reengage by Another_campaign_rec.
execute.
