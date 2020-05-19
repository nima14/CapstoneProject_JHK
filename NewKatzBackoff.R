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
# Input17 <- "Sell the"





Sentence <- "Sell the"
gamma3 <- 0.5
gamma2 <- 0.5


MakeNGrams <- function(Sentence)
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
  
  
  
  Inp_1g <-  data.table(x[1] )  
             for(i in 2:4)
                {
                  assign(paste("Inp_",i,"g",sep=''),data.table(str_split_fixed(x[i]," ", i)),
                         envir = .GlobalEnv) 
             }
  
  MyList <- list(Inp_1g,Inp_2g,Inp_3g,Inp_4g)
  return(MyList)
}




GetObsProbs <- function(Sentence,gamma2,gamma3){
  
                ngrams <- MakeNGrams(Sentence)
                Inp_1g <- ngrams[[1]]
                Inp_2g <- ngrams[[2]]
                Inp_3g <- ngrams[[3]]
                Inp_4g <- ngrams[[4]]
#------------------------------------------------------------               
                obs_3 <- data.table(ThreeG_Words[V1==Inp_2g[[1]] & V2==Inp_2g[[2]],c(3,4)] %>% arrange(desc(N)))
                TotalSum_3 <- sum(obs_3$N)
                obsProb_3 <- data.table(obs_3$V3,(obs_3$N-gamma3)/TotalSum_3)
                alpha_3 <- 1 - sum(obsProb_3$V2)
#------------------------------------------------------------
                obs_2 <- data.table(TwoG_Words[V1==Inp_1g[[1]],c(2,3)] %>% arrange(desc(N)))
                TotalSum_2 <- sum(obs_2$N)
                obsProb_2 <- data.table(obs_2$V2,(obs_2$N-gamma2)/TotalSum_2)
                alpha_2 <- 1 - sum(obsProb_2$V2)
#------------------------------------------------------------
                TotalWords <- data.table(OneG_Words[,c(1,2)] %>% arrange(desc(N)))
                unobs_1 <- TotalWords[!(V1 %in% obs_2$V2),]
                TotalSum_1 <- sum(unobs_1$N)
                Prob_1 <- data.table(unobs_1$V1,alpha_2*unobs_1$N/TotalSum_1)
#------------------------------------------------------------
                Total2GNot3G <- rbind(obsProb_2[!(V1 %in% obsProb_3$V1)],Prob_1)
                TotalSumProb_2 <- sum(Total2GNot3G$V2)
                Prob_2 <- data.table(Total2GNot3G$V1,alpha_3*Total2GNot3G$V2/TotalSumProb_2)
#------------------------------------------------------------              
                Prob_3 <- rbind(obsProb_3,Prob_2)
                top_n(Prob_3,5,V2)
                
}


GetObsProbs(Input4,0.5,0.5)
