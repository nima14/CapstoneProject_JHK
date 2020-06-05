library(dplyr)
library(stringi)
library(stringr)
library(data.table)
library(quanteda)
library(tm)
TrainTwitter <- read.table("TrainTwitter.txt",header = TRUE,fill=TRUE) %>% mutate(Source='Twitter')
 
TrainBlogs <- read.table("TrainBlogs.txt",header = TRUE,fill=TRUE)  %>% mutate(Source='Blogs')

TrainNews <- read.table("TrainNews.txt",header = TRUE,fill=TRUE)  %>% mutate(Source='News')

Train <- rbind(TrainTwitter,TrainBlogs,TrainNews)
rm(TrainTwitter,TrainBlogs,TrainNews)


#Delete unkown characters.
Train<- data.table(iconv(Train$x, "UTF-8", "UTF-8",sub=''))


#Delete break lines.
Train <-  data.table(str_replace_all(Train$V1, "[\r\n]" , " "))


# Lowercase
Train$V1 <- tolower(Train$V1)
#Removing punctuations and numbers
Train$V1 <- gsub('[[:punct:] ]+',' ',Train$V1)
Train$V1 <- gsub('[0-9]+', '', Train$V1)

#Remove Profanity words
ProfanityWords <- read.table("Profanity.txt",sep="\n",quote = "")
Train$V1 <- removeWords(Train$V1,ProfanityWords$V1)

rm(ProfanityWords)

#Creating Corpus
Corp <- corpus(as.character(Train$V1))


#Create tokens
Tok <- data.table(tokens(Corp))



# Building 2-gram & 3-grams

Twogram <-  data.table(tokens_ngrams(Tok$V1, n =2, concatenator = " "))

Threegram <-  data.table(tokens_ngrams(Tok$V1, n =3, concatenator = " "))

Fourgram <-  data.table(tokens_ngrams(Tok$V1, n =4, concatenator = " "))

Fivegram <-  data.table(tokens_ngrams(Tok$V1, n =5, concatenator = " "))


## Cleaning the Tokens

FreqTok <- data.table( table(as.character(Tok$V1))  )
OneG_Words <- data.table(FreqTok[FreqTok$N>1])

rm(FreqTok)

FreqTokTwoGram <- data.table( table(as.character(Twogram$V1))  )
Base_TwoGram <- data.table(FreqTokTwoGram[FreqTokTwoGram$N>1])
TwoG_Words <- data.table(str_split_fixed(Base_TwoGram$V1," ", 2),Base_TwoGram$N)
names(TwoG_Words) <- c("V1","V2","N")
rm(FreqTokTwoGram,Base_TwoGram)


FreqTokThreeGram <- data.table( table(as.character(Threegram$V1))  )
Base_ThreeGram <- data.table(FreqTokThreeGram[FreqTokThreeGram$N>1])
ThreeG_Words <- data.table(str_split_fixed(Base_ThreeGram$V1," ", 3),Base_ThreeGram$N)
names(ThreeG_Words) <- c("V1","V2","V3","N")
rm(FreqTokThreeGram,Base_ThreeGram)


FreqTokFourGram <- data.table( table(as.character(Fourgram$V1))  )
Base_FourGram <- data.table(FreqTokFourGram[FreqTokFourGram$N>1])
FourG_Words <- data.table(str_split_fixed(Base_FourGram$V1," ", 4),Base_FourGram$N)
names(FourG_Words) <- c("V1","V2","V3","V4","N")
rm(FreqTokFourGram,Base_FourGram)


FreqTokFiveGram <- data.table( table(as.character(Fivegram$V1))  )
Base_FiveGram <- data.table(FreqTokFiveGram[FreqTokFiveGram$N>1])
FiveG_Words <- data.table(str_split_fixed(Base_FiveGram$V1," ", 5),Base_FiveGram$N)
names(FiveG_Words) <- c("V1","V2","V3","V4","V5","N")
rm(FreqTokFiveGram,Base_FiveGram)


##Inetgration of Ngrams


names(OneG_Words) <- c("Pred","N")


names(TwoG_Words) <- c("Prefix","Pred","N")


ThreeG_Words <- data.table(paste(ThreeG_Words$V1,ThreeG_Words$V2),ThreeG_Words$V3,ThreeG_Words$N)
names(ThreeG_Words) <- c("Prefix","Pred","N")


FourG_Words <- data.table(paste(FourG_Words$V1,FourG_Words$V2,FourG_Words$V3),FourG_Words$V4,FourG_Words$N)
names(FourG_Words) <- c("Prefix","Pred","N")


FiveG_Words <- data.table(paste(FiveG_Words$V1,FiveG_Words$V2,FiveG_Words$V3,FiveG_Words$V4),FiveG_Words$V5,FiveG_Words$N)
names(FiveG_Words) <- c("Prefix","Pred","N")



Ngram_Words <- rbind(data.table(Prefix="",OneG_Words,ngram=1),
                     data.table(TwoG_Words,ngram=2),
                     data.table(ThreeG_Words,ngram=3),
                     data.table(FourG_Words,ngram=4),
                     data.table(FiveG_Words,ngram=5))

setkey(Ngram_Words,Prefix,ngram)


rm(OneG_Words,TwoG_Words,ThreeG_Words,FourG_Words,FiveG_Words)
rm(Tok,Twogram,Threegram,Fourgram,Fivegram,Train)

# Save the files

saveRDS(Ngram_Words,"Ngram_Words.Rdata")









