nomatch<-read_csv('~/Documents/TurboVote/Matching Self-Reported/Not matched no dup_email fix.csv')

#identify people with no email
nomatch_noemail<-nomatch%>%
  filter(is.na(email))

# processReferralColumn <- function(nomatch) {

#parse out nsids
  maxSep <- max(as.numeric(names(table(str_count(nomatch_noemail$referralcode, ',')))))+1
  parsedSep <-
    nomatch_noemail %>%
    select(id, referralcode) %>%
    separate(referralcode, LETTERS[1:maxSep], ',',remove = T) %>%
    mutate(
      nsid =
        case_when(
          substr(A, 1, 4)=='user' ~ substr(A, 6, nchar(A)),
          TRUE ~ ''
        ))

  nomatch_noemail <- parsedSep%>%
    select(nsid)

write.csv(nomatch_noemail, file = 'TurboVote nsids no email.csv')
