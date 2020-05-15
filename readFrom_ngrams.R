library(dplyr)
library(stringi)
library(stringr)
library(data.table)
library(quanteda)

-----------------------------------------------------------
OneG_Words   <- readRDS("OneG_Words.Rdata")
TwoG_Words   <- readRDS("TwoG_Words.Rdata")
ThreeG_Words <- readRDS("ThreeG_Words.Rdata")
FourG_Words  <- readRDS("FourG_Words.Rdata")
FiveG_Words  <- readRDS("FiveG_Words.Rdata")


# Input <- "Somebody told me to go there but I Refused to do so"
# Input2 <- "Hello there"
# Input3 <- "I do not work there"
# Input4 <- "me"
# Input5 <- "I went to Johns Hopkins"
# Input6 <- "Let's have a look at"
# Input7 <-"The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
# Input8 <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
# Input9 <- "Hey sunshine, can you follow me and make me the"
# Input10 <- "Very early observations on the Bills game: Offense still struggling but the"
# Input11 <- "Go on a romantic date at the"
# Input12 <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
# Input13 <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
# Input14 <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
# Input15 <- "Be grateful for the good times and keep the faith during the"
# Input16 <- "If this isn't the cutest thing you've ever seen, then you must be"
MakeNGrams <- function(Sentence)
{
  Sentence <- tolower(Sentence) 
  x <<- as.character( rep("",4))
  len <-  sapply(strsplit(Sentence, " "), length)
  
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



MakeNGrams(Input16)



FiveG_Words[V1==Inp_4g[[1]] & V2==Inp_4g[[2]] &
              V4== Inp_3g[[3]]& V2==Inp_4g[[4]] ,c(5,6) ]%>% arrange(N)


FourG_Words[V1==Inp_3g[[1]] & V2==Inp_3g[[2]] &
              V3== Inp_3g[[3]],c(4,5) ]%>% arrange(N)

ThreeG_Words[V1==Inp_2g[[1]] & V2==Inp_2g[[2]],c(3,4)] %>% arrange(N)

TwoG_Words[V1==Inp_1g[[1]] ,c(2,3)] %>% arrange(N)

OneG_Words %>% arrange(N)
