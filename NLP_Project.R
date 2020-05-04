library(quanteda)

setwd("N:/R Courses/CapstoneProject_JHK")

TrainTwitter <- read.table("TrainTwitter.txt")
ProfanityWords <- read.table("Profanity.txt",sep="\n")
CorpTwitter <- corpus(as.character(TrainTwitter$x))
  TokTwitterProfane <- tokens(CorpTwitter)
  
TokTwitter <- tokens_select(TokTwitter, ProfanityWords, selection = "remove", padding = FALSE)