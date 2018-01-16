source('config/init.R')
library(httr)
library(jsonlite)

### Grab API key from environment var
key <- Sys.getenv('TYPEFORM_KEY')

# Get a list of all our typeforms and their unique IDs
allForms <- paste0('https://api.typeform.com/v1/forms?key=',key)
res <- GET(url = allForms)
json <- httr::content(res, as = "text")
allTypeForms <- fromJSON(json)

# Grab the ID for the DoSomething Feedback survey
allTypeForms %>% 
  filter(name == 'DoSomething Feedback') %>% 
  select(id) %>% as.character() -> feedback

# Submit request for that survey
npsfeedback <- paste0('https://api.typeform.com/v1/form/',feedback,'?key=',key)
res <- GET(url = npsfeedback)
json <- content(res, as = "text")
feedbackResults <- fromJSON(json)

# Grab questions and answers (including hidden fields) from request
questions <- as.tibble(feedbackResults$questions)
answers <- as.tibble(cbind(feedbackResults$responses$hidden, feedbackResults$responses$answers))
