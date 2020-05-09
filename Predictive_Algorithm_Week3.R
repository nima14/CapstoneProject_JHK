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


#Create tokwns + Remove numbers & punctuations
Tok <- data.table(tokens(Corp$x,remove_punct = TRUE, remove_numbers =  TRUE,
                     remove_symbols = TRUE),Train$Source)
names(Tok) <- c("x","Source")


#Lowercase all the characters.
Tok <- data.table(tokens_tolower(Tok$x) , Tok$Source )
names(Tok) <- c("x","Source")

#Remove profane words.
ProfanityWords <- read.table("Profanity.txt",sep="\n",quote = "")

Tok <- data.table(tokens_select(Tok$x,c(stopwords("en"),ProfanityWords$V1),
                            selection = "remove", padding = FALSE) , Tok$Source )
names(Tok) <- c("x","Source")

rm(ProfanityWords)

#Lemmatization
Tok <- data.table(tokens_wordstem(Tok$x) , Tok$Source )
names(Tok) <- c("x","Source")



# Building 2-gram & 3-grams

Twogram <-  data.table(tokens_ngrams(Tok$x, n =2, concatenator = " "), Tok$Source)
names(Tok) <- c("x","Source")


Threegram <-  data.table(tokens_ngrams(Tok$x, n =3, concatenator = " "), Tok$Source)
names(Tok) <- c("x","Source")



