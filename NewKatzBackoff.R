library(dplyr)
library(stringi)
library(stringr)
library(data.table)
library(quanteda)

-----------------------------------------------------------


OneG_Words   <- readRDS("OneG_Words2.Rdata")
TwoG_Words   <- readRDS("TwoG_Words2.Rdata")
ThreeG_Words <- readRDS("ThreeG_Words2.Rdata")
FourG_Words  <- readRDS("FourG_Words2.Rdata")
FiveG_Words  <- readRDS("FiveG_Words2.Rdata")


names(OneG_Words) <- c("Pred","N")


names(TwoG_Words) <- c("Prefix","Pred","N")


ThreeG_Words <- data.table(paste(ThreeG_Words$V1,ThreeG_Words$V2),ThreeG_Words$V3,ThreeG_Words$N)
names(ThreeG_Words) <- c("Prefix","Pred","N")


FourG_Words <- data.table(paste(FourG_Words$V1,FourG_Words$V2,FourG_Words$V3),FourG_Words$V4,FourG_Words$N)
names(FourG_Words) <- c("Prefix","Pred","N")


FiveG_Words <- data.table(paste(FiveG_Words$V1,FiveG_Words$V2,FiveG_Words$V3,FiveG_Words$V4),FiveG_Words$V5,FiveG_Words$N)
names(FiveG_Words) <- c("Prefix","Pred","N")


setkey(TwoG_Words,Prefix)
setkey(ThreeG_Words,Prefix)
setkey(FourG_Words,Prefix)
setkey(FiveG_Words,Prefix)


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
# Input17 <- "Sell the"
# Input18 <- "This Video will be"
# Input19 <- "have to put on"




Sentence <- Input18
gamma2 <- 0.5
gamma3 <- 0.5
gamma4 <- 0.5
gamma5 <- 0.5



MakeNGrams2 <- function(Sentence)
{
  Sentence <- tolower(Sentence) 
  x <- as.character( rep("",4))
  len <-  sapply(strsplit(Sentence, " "), length)
  
  for(i in 0:min(3,len-1))
  {
    Start <- len-i
    End <- len
    x[i+1] <- word(Sentence,Start,End)
  }
  
  
  for(i in 1:4)
  {
    assign(paste("Inp_",i,"g",sep=''),data.table(x[i]),
           envir = .GlobalEnv) 
  }
  
  MyList <- list(Inp_1g,Inp_2g,Inp_3g,Inp_4g)
  return(MyList)
}

Sentence <- "I can not lie about what i hear"







GetObsProbs <- function(Sentence,gamma2=0.5,gamma3=0.5,gamma4=0.5,gamma5=0.5){
  
  ngrams <- MakeNGrams2(Sentence)
  Inp_1g <- ngrams[[1]]
  Inp_2g <- ngrams[[2]]
  Inp_3g <- ngrams[[3]]
  Inp_4g <- ngrams[[4]]
  
  #------------------------------------------------------------               
  obs_5 <- data.table(Ngram_Words3[Ngram_Words3$Prefix==Inp_4g[[1]] & 
                                     Ngram_Words3$ngram==5 ,c(2,4)] %>%
                        arrange(desc(N)))
  TotalSum_5 <- sum(obs_5$N)
  obsProb_5 <- data.table(obs_5$Pred,(obs_5$N-gamma5)/TotalSum_5)
  alpha_5 <- 1 - sum(obsProb_5$V2)
  #------------------------------------------------------------               
  obs_4 <- data.table(Ngram_Words3[Ngram_Words3$Prefix==Inp_3g[[1]] & 
                                     Ngram_Words3$ngram==4 ,c(2,4)] %>%
                        arrange(desc(N)))
  TotalSum_4 <- sum(obs_4$N)
  obsProb_4 <- data.table(obs_4$Pred,(obs_4$N-gamma4)/TotalSum_4)
  alpha_4 <- 1 - sum(obsProb_4$V2)
  #------------------------------------------------------------               
  obs_3 <- data.table(Ngram_Words3[Ngram_Words3$Prefix==Inp_2g[[1]] & 
                                     Ngram_Words3$ngram==3 ,c(2,4)] %>%
                        arrange(desc(N)))
  TotalSum_3 <- sum(obs_3$N)
  obsProb_3 <- data.table(obs_3$Pred,(obs_3$N-gamma3)/TotalSum_3)
  alpha_3 <- 1 - sum(obsProb_3$V2)
  #------------------------------------------------------------
  obs_2 <- data.table(Ngram_Words3[Ngram_Words3$Prefix==Inp_1g[[1]] & 
                                     Ngram_Words3$ngram==2 ,c(2,4)] %>%
                        arrange(desc(N)))
  TotalSum_2 <- sum(obs_2$N)
  obsProb_2 <- data.table(obs_2$Pred,(obs_2$N-gamma2)/TotalSum_2)
  alpha_2 <- 1 - sum(obsProb_2$V2)
  #------------------------------------------------------------
  TotalWords <- data.table(Ngram_Words3[Ngram_Words3$ngram==1,c(2,4)] %>% arrange(desc(N)))
  unobs_1 <- TotalWords[!(Pred %in% obs_2$Pred),]
  TotalSum_1 <- sum(unobs_1$N)
  Prob_1 <- data.table(unobs_1$Pred,alpha_2*unobs_1$N/TotalSum_1)
  #------------------------------------------------------------
  Total2GNot3G <- rbind(obsProb_2[!(V1 %in% obsProb_3$V1)],Prob_1)
  TotalSumProb_2 <- sum(Total2GNot3G$V2)
  Prob_2 <- data.table(Total2GNot3G$V1,alpha_3*Total2GNot3G$V2/TotalSumProb_2)
  #------------------------------------------------------------    
  Total3GNot4G <- rbind(obsProb_3[!(V1 %in% obsProb_4$V1)],Prob_2)
  TotalSumProb_3 <- sum(Total3GNot4G$V2)
  Prob_3 <- data.table(Total3GNot4G$V1,alpha_4*Total3GNot4G$V2/TotalSumProb_3)
  #------------------------------------------------------------  
  Total4GNot5G <- rbind(obsProb_4[!(V1 %in% obsProb_5$V1)],Prob_3)
  TotalSumProb_4 <- sum(Total4GNot5G$V2)
  Prob_4 <- data.table(Total4GNot5G$V1,alpha_5*Total4GNot5G$V2/TotalSumProb_4)
  #------------------------------------------------------------  
  Prob_5 <- rbind(obsProb_5,Prob_4)
  top_n(Prob_5,5,V2)
  
}



