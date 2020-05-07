library(quanteda)
library(dplyr)
library(ggplot2)
library(read)

Path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(Path)

TrainTwitter <- read.table("TrainTwitter.txt",header = TRUE,fill=TRUE)
#Delete unkown characters.
TrainTwitter <- iconv(TrainTwitter, "UTF-8", "UTF-8",sub='') 
ProfanityWords <- read.table("Profanity.txt",sep="\n",quote = "")
CorpTwitter <- corpus(as.character(TrainTwitter$x))
TokTwitter <- tokens(CorpTwitter,remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
  
TokTwitter <- tokens_select(TokTwitter,c(stopwords("en"),ProfanityWords$V1),
                            selection = "remove", padding = FALSE)



-------------------------------------------------

FreqTok <- as.data.frame(table(as.character(TokTwitter)) )

FreqTok <- arrange(FreqTok,desc(FreqTok$Freq))

#60% have 1 freq!
mean(FreqTok$Freq==1)

-------------------------------------------------------
  
  
Twogram <-  tokens_ngrams(TokTwitter, n =2, concatenator = " ")


FreqTokTwoGram <- as.data.frame(table(as.character(Twogram)) )

FreqTokTwoGram <- arrange(FreqTokTwoGram,desc(FreqTokTwoGram$Freq))


Threegram <- tokens_ngrams(TokTwitter, n =3, concatenator = " ")

FreqTokThreeGram <- as.data.frame(table(as.character(Threegram)) )

FreqTokThreeGram <- arrange(FreqTokThreeGram,desc(FreqTokThreeGram$Freq))


-------------------------------
  
FreqTokTwoGram %>% top_n(20) %>% as.data.frame() %>%
  ggplot( aes(Var1, Freq))+ geom_col()

qplot(Threegram, geom="histogram") 

ggplot(df, aes(x, y)) + geom_point()

-----------------------------------------
  
x <- dfm(TokTwitter,stem=TRUE,tolower = TRUE)
topfeatures(x,20)
