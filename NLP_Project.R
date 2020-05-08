library(quanteda)
library(dplyr)
library(ggplot2)
library(read)
library(stringi)
library(stringr)
Path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(Path)

TrainTwitter <- read.table("TrainTwitter.txt",header = TRUE,fill=TRUE)
#Delete unkown characters.
TrainTwitter$x <- iconv(TrainTwitter$x, "UTF-8", "UTF-8",sub='') 
TrainTwitter$x <- str_replace_all(TrainTwitter$x, "[\r\n]" , " ")

ProfanityWords <- read.table("Profanity.txt",sep="\n",quote = "")
CorpTwitter <- corpus(as.character(TrainTwitter$x))
TokTwitter <- tokens(CorpTwitter,remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
TokTwitter <- tokens_tolower(TokTwitter)  
TokTwitter <- tokens_select(TokTwitter,c(stopwords("en"),ProfanityWords$V1),
                            selection = "remove", padding = FALSE)

TokTwitter <- tokens_wordstem(TokTwitter)



-------------------------------------------------

FreqTok <- as.data.frame(table(as.character(TokTwitter)) )

FreqTok <- arrange(FreqTok,desc(FreqTok$Freq))

#61% have 1 freq!
mean(FreqTok$Freq==1)

-------------------------------------------------------
  
  
Twogram <-  tokens_ngrams(TokTwitter, n =2, concatenator = " ")


FreqTokTwoGram <- as.data.frame(table(as.character(Twogram)) )

FreqTokTwoGram <- arrange(FreqTokTwoGram,desc(FreqTokTwoGram$Freq))


Threegram <- tokens_ngrams(TokTwitter, n =3, concatenator = " ")

FreqTokThreeGram <- as.data.frame(table(as.character(Threegram)) )

FreqTokThreeGram <- arrange(FreqTokThreeGram,desc(FreqTokThreeGram$Freq))


-------------------------------
  
FreqTokThreeGram %>% top_n(20) %>% as.data.frame() %>%
    ggplot( aes(reorder(Var1,-Freq), Freq))+ geom_col()+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+ 
        labs(x = "3-Gram",y="Frequency")


FreqTokTwoGram %>% top_n(20) %>% as.data.frame() %>%
  ggplot( aes(reorder(Var1,-Freq), Freq))+ geom_col()+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+ 
  labs(x = "2-Gram",y="Frequency")
  



-----------------------------------------
  
  
#Total number of words
sum(sapply(strsplit(TrainTwitter$x[c(4,5)], " "), length))

stri_stats_latex(TrainTwitter$x)[[4]]

#Mean of number of words in each record
mean(sapply(strsplit(TrainTwitter$x, " "), length))

stri_stats_latex(TrainTwitter$x)[[4]]/
  stri_stats_general(TrainTwitter$x)[[1]]


#Total number of characters
sum(sapply(strsplit(TrainTwitter$x, ""), length))

stri_stats_latex(TrainTwitter$x)[[1]]
#Mean of number of characters in each record.
mean(sapply(strsplit(TrainTwitter$x, ""), length))


stri_stats_latex(TrainTwitter$x)[[1]]/
  stri_stats_general(TrainTwitter$x)[[1]]







