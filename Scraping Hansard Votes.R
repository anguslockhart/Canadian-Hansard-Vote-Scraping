  #This scrapes up to March 5th, 2018.  Update vote totals, session numbers and parliament numbers as needed.

number_of_sessions <- c(1,2,3,2,1)
number_of_votes <- c(190, 219, 161, 1, 158, 204, 760, 467, 457)
links <- c()

l <- 0
for (i in 38:42) {
  for (j in 1:number_of_sessions[(i-37)]) {
    l <- l+1
    for (k in 1:number_of_votes[l]){
      links <- c(links, paste0("http://www.ourcommons.ca/Parliamentarians/en/votes/", i, "/", j, "/", k))
    }
  }
}

votescraping <- function(link, oldpage=NULL){
  require(XML)
  require(tidyverse)
  vote <- readLines(link)
  vote <- gsub('<img class=\"voteDetailsImg\" alt=\"Nay\" src=\"/Parliamentarians/Content/images/black_x.gif\">', "<font size='2'>Nay&nbsp;</font>", vote) 
  vote <- gsub('<img class=\"voteDetailsImg\" alt=\"Yea\" src=\"/Parliamentarians/Content/images/black_x.gif\">', "<font size='2'>Yea&nbsp;</font>", vote) 
  date <- as.data.frame(vote[grepl("Journals of", vote)]) %>%
    separate(1, into = c("junk", "date"), sep="Journals of ") %>%
    separate(date, into = c("date", "morejunk"), sep=" <")
  
  votenum <- as.data.frame(vote[grepl("Vote No.  ", vote)]) %>%
    separate(1, into = c("junk", "vote"), sep="Vote No.  ") %>%
    separate(vote, into = c("votenum", "morejunk"), sep="</span")
  
  subject <- as.data.frame(vote[grepl("voteDetailsTopHeaderContent", vote)][3])
  
  vote <- readHTMLTable(vote)
  vote <- vote[[2]]
  
  metadata <- as.data.frame(link) %>%
    separate(1, "/", into = c("http", "blank", "ourcommons", "parl", "language","votes", "parliament", "session", "vote"), extra="merge") %>%
    select(parliament, session, vote)
  
  vote <- data.frame(metadata, rep(date$date, length(vote[[1]])), rep(votenum$votenum, length(vote[[1]])), rep(subject[[1]], length(vote[[1]])), vote)
  
  if (!is.null(oldpage)) {
    vote <- rbind(oldpage, vote)
  }
  return(vote)
}

for (i in 1:length(links)){
  if (i==1) {
    try(
      oldpage <- votescraping(links[i]), 
      silent=TRUE)
  }
  if (i!=1) {
    try(
      oldpage <- votescraping(links[i], oldpage=oldpage), 
      silent=TRUE)
  }
  print(i)
}


write.csv("thesisdata.csv", oldpage)
