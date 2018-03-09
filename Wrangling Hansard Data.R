  ###Wrangling Data###

  ##Make the names of variables better
names(oldpage) <- c("parliament", "session", "vote", "date", "vote2", "subject", "name_riding", "party", "yea", "nay", "paired")

  ##Split out names and constituencies

data <- separate(oldpage, name_riding, into=c("name", "riding"), sep="\\(")
data$riding <- gsub('.{1}$', '', data$riding)
  ##Identifying Party Line##

partyline <- group_by(data, parliament, session, vote, party) %>%
  summarize("num_votes" = n(),
            "num_yea" = sum(yea=="Yea", na.rm=T),
            "num_nay" = sum(nay=="Nay", na.rm=T)) %>%
  filter(party!="Independent")


partyvote <- rep("neither", length(partyline[[1]]))
partyvote[partyline$num_yea>partyline$num_nay] <- "yea"
partyvote[partyline$num_yea<partyline$num_nay] <- "nay"

partyline <- data.frame(partyline, partyvote)
          