* Encoding: UTF-8.
*Check data.
CROSSTABS
  /TABLES=NPS_rec Value Causes Communication Website Campaigns Scholarships Community DS_interact
    Participation Another_campaign Age NPS_cat BY Survey
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ
  /CELLS=COUNT COLUMN
  /COUNT ROUND CELL.

*Freqs.
FREQUENCIES VARIABLES=NPS_cat Value Causes Communication Website Campaigns Scholarships Community
    DS_interact Participation Another_campaign Age_rec
  /ORDER=ANALYSIS.

*Averages.
DESCRIPTIVES VARIABLES=Causes Communication Website Campaigns Scholarships Community
  /STATISTICS=MEAN STDDEV MIN MAX.

*NPS.
CROSSTABS
  /TABLES=Value Causes Communication Website Campaigns Scholarships Community DS_interact
    Participation Another_campaign Age BY NPS_cat
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ
  /CELLS=COUNT COLUMN
  /COUNT ROUND CELL.

*Likelihood and satisfaction.
CROSSTABS
  /TABLES=Causes Communication Website Campaigns Scholarships Community BY Another_campaign
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ
  /CELLS=COUNT ROW
  /COUNT ROUND CELL.

*Email vs. SMS.
SORT CASES  BY Survey_type.
SPLIT FILE LAYERED BY Survey_type.

CROSSTABS
  /TABLES=DS_interact Participation Another_campaign BY NPS_rec
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

*re-engagement among 4/5 stars.
CROSSTABS
  /TABLES=Causes Communication Website Campaigns Scholarships Community BY Another_campaign
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW
  /COUNT ROUND CELL.








