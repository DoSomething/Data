library(jsonlite)

test <- fromJSON('~/Downloads/59231f0c95cfc9addc248126-action-2017-08-07T21-00-00.000Z.json')



"APPLICATION_INIT" = this is their first landing page in a session

1) How to get every "first page"
  - Application INIT in action.type & thisCampaign = FALSE
2) Any actions in that same session.id & after application_init
  - Find action.type = "SIGNUP_CREATED"