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



# Save the files

saveRDS(OneG_Words,"OneG_Words.Rdata")
saveRDS(TwoG_Words,"TwoG_Words.Rdata")
saveRDS(ThreeG_Words,"ThreeG_Words.Rdata")
saveRDS(FourG_Words,"FourG_Words.Rdata")
saveRDS(FiveG_Words,"FiveG_Words.Rdata")


# Input <- "Somebody told me to go there but I Refused to do so"
# Input2 <- "Hello there"
# Input3 <- "I do not work there"
# Input4 <- "me"
# Input5 <- "I went to Johns Hopkins"



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

         

    Inp_1g <<-  data.table(x[1] )  
    for(i in 2:4)
    {
      assign(paste("Inp_",i,"g",sep=''),data.table(str_split_fixed(x[i]," ", i)),
                            envir = .GlobalEnv) 
    }
}



rm(Corp,Fivegram,Fourgram,Threegram,Tok,Train,Twogram)



MakeNGrams(Input5)



FiveG_Words[V1==Inp_4g[[1]] & V2==Inp_4g[[2]] &
            V4== Inp_3g[[3]]& V2==Inp_4g[[4]] ,c(5,6) ]


FourG_Words[V1==Inp_3g[[1]] & V2==Inp_3g[[2]] &
              V3== Inp_3g[[3]],c(4,5) ]

ThreeG_Words[V1==Inp_2g[[1]] & V2==Inp_2g[[2]],c(3,4)]

TwoG_Words[V1==Inp_1g[[1]] ,c(2,3)]

OneG_Words






