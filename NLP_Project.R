library(quanteda)
library(dplyr)
setwd("C:/Users/n.taghidoost/Desktop/RFunctions/CapstoneProject_JHK")

TrainTwitter <- read.table("TrainTwitter.txt")
ProfanityWords <- read.table("Profanity.txt",sep="\n",quote = "")
CorpTwitter <- corpus(as.character(TrainTwitter$x))
  TokTwitterProfane <- tokens(CorpTwitter,remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
  
TokTwitter <- tokens_select(TokTwitterProfane,c(stopwords("en"),ProfanityWords$V1),
                            selection = "remove", padding = FALSE)

#a <- cbind(as.character(TrainTwitter[grep("fuck",TrainTwitter$x),]),grep("fuck",TrainTwitter$x))



-------------------------------------------------

FreqTok <- as.data.frame(table(as.character(TokTwitter)) )

FreqTok <- arrange(FreqTok,desc(FreqTok$Freq))

#60% have 1 freq!
mean(FreqTok$Freq==1)

