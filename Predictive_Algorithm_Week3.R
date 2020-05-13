library(dplyr)
library(stringi)
library(stringr)
library(data.table)
library(quanteda)
TrainTwitter <- read.table("TrainTwitter.txt",header = TRUE,fill=TRUE) %>% mutate(Source='Twitter')
 
TrainBlogs <- read.table("TrainBlogs.txt",header = TRUE,fill=TRUE)  %>% mutate(Source='Blogs')

TrainNews <- read.table("TrainNews.txt",header = TRUE,fill=TRUE)  %>% mutate(Source='News')

Train <- rbind(TrainTwitter,TrainBlogs,TrainNews)
rm(TrainTwitter,TrainBlogs,TrainNews)


#Delete unkown characters.
Train<- data.table(iconv(Train$x, "UTF-8", "UTF-8",sub=''),Train$Source)
names(Train) <- c("x","Source")

#Delete break lines.
Train <-  data.table(str_replace_all(Train$x, "[\r\n]" , " "),Train$Source)
names(Train) <- c("x","Source")


#Creating Corpus
Corp <- data.table(corpus(as.character(Train$x)),Train$Source)
names(Corp) <- c("x","Source")


#Create tokens + Remove numbers & punctuations
Tok <- data.table(tokens(Corp$x,remove_punct = TRUE, remove_numbers =  TRUE,
                     remove_symbols = TRUE),Train$Source)
names(Tok) <- c("x","Source")


#Lowercase all the characters.
Tok <- data.table(tokens_tolower(Tok$x) , Tok$Source )
names(Tok) <- c("x","Source")

#Remove profane words.
ProfanityWords <- read.table("Profanity.txt",sep="\n",quote = "")

Tok <- data.table(tokens_select(Tok$x,ProfanityWords$V1,
                            selection = "remove", padding = FALSE) , Tok$Source )
names(Tok) <- c("x","Source")

rm(ProfanityWords)

#Lemmatization

# Tok <- data.table(tokens_wordstem(Tok$x) , Tok$Source )
# names(Tok) <- c("x","Source")



# Building 2-gram & 3-grams

Twogram <-  data.table(tokens_ngrams(Tok$x, n =2, concatenator = " "), Tok$Source)
names(Tok) <- c("x","Source")



Threegram <-  data.table(tokens_ngrams(Tok$x, n =3, concatenator = " "), Tok$Source)
names(Tok) <- c("x","Source")


Fourgram <-  data.table(tokens_ngrams(Tok$x, n =4, concatenator = " "), Tok$Source)
names(Tok) <- c("x","Source")

Fivegram <-  data.table(tokens_ngrams(Tok$x, n =5, concatenator = " "), Tok$Source)
names(Tok) <- c("x","Source")



## Cleaning the Tokens

FreqTok <- data.table( table(as.character(Tok$x))  )
Base_Tok <- FreqTok[FreqTok$N>1]
rm(FreqTok)

FreqTokTwoGram <- data.table( table(as.character(Twogram$V1))  )
Base_TwoGram <- FreqTokTwoGram[FreqTokTwoGram$N>1]
rm(FreqTokTwoGram)


FreqTokThreeGram <- data.table( table(as.character(Threegram$V1))  )
Base_ThreeGram <- FreqTokThreeGram[FreqTokThreeGram$N>1]
rm(FreqTokThreeGram)


FreqTokFourGram <- data.table( table(as.character(Fourgram$V1))  )
Base_FourGram <- FreqTokFourGram[FreqTokFourGram$N>1]
rm(FreqTokFourGram)


FreqTokFiveGram <- data.table( table(as.character(Fivegram$V1))  )
Base_FiveGram <- FreqTokFiveGram[FreqTokFiveGram$N>1]
rm(FreqTokFiveGram)

# Input <- "Somebody told me to go there but I Refused to do so"
# Input2 <- "Hello there"
# Input3 <- "I do not work there"
# Input4 <- "me"




MakeNGrams <- function(Sentence)
     {
         Sentence <- tolower(Sentence) 
         x <<- as.character( rep("",4))
         len <- stri_stats_latex(Sentence)[[4]]
            
            for(i in 0:min(3,len-1))
            {
                  Start <- len-i
                  End <- len
                  x[i+1] <<- word(Sentence,Start,End)
              
            }
            
            
  
}

MakeNGrams(Input)

Base_FiveGram[match(x[4],word(Base_FiveGram$V1,1,4))]
Base_FourGram[match(x[3],word(Base_FourGram$V1,1,3))]
Base_ThreeGram[match(x[2],word(Base_ThreeGram$V1,1,2))]
Base_TwoGram[match(x[1],word(Base_TwoGram$V1,1,1))]
