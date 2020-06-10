library(dplyr)
library(stringi)
library(stringr)
library(data.table)
library(quanteda)

#---------------------------------------------------------------------

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
    
    
    for(i in 1:4)
    {
      assign(paste("Inp_",i,"g",sep=''),data.table(x[i]),
             envir = .GlobalEnv) 
    }
    
    MyList <- list(Inp_1g,Inp_2g,Inp_3g,Inp_4g)
    return(MyList)
  }
  

 #---------------------------------------------------------------------
GetObsProbs <- function(Sentence,gamma2=2,gamma3=0.5,gamma4=0.5,gamma5=0.5){
  
  ngrams <- MakeNGrams(Sentence)
  Inp_1g <- ngrams[[1]]
  Inp_2g <- ngrams[[2]]
  Inp_3g <- ngrams[[3]]
  Inp_4g <- ngrams[[4]]
 
  #------------------------------------------------------------               
  obs_5 <- Ngram_Words[Ngram_Words$Prefix==Inp_4g[[1]] & 
                                     Ngram_Words$ngram==5 ,c(2,3)] %>%
                        arrange(desc(N)) %>% data.table()
  TotalSum_5 <- sum(obs_5$N)
  obsProb_5 <- data.table(obs_5$Pred,(obs_5$N-gamma5)/TotalSum_5)
  alpha_5 <- 1 - sum(obsProb_5$V2)
  #------------------------------------------------------------               
  obs_4 <- Ngram_Words[Ngram_Words$Prefix==Inp_3g[[1]] & 
                                     Ngram_Words$ngram==4 ,c(2,3)] %>%
                        arrange(desc(N)) %>% data.table()
  TotalSum_4 <- sum(obs_4$N)
  obsProb_4 <- data.table(obs_4$Pred,(obs_4$N-gamma4)/TotalSum_4)
  alpha_4 <- 1 - sum(obsProb_4$V2)
  #------------------------------------------------------------  
  obs_3 <- data.table(Ngram_Words[Ngram_Words$Prefix==Inp_2g[[1]] & 
                                    Ngram_Words$ngram==3 ,c(2,3)] %>%
                        arrange(desc(N)))
 
  TotalSum_3 <- sum(obs_3$N)
  obsProb_3 <- data.table(obs_3$Pred,(obs_3$N-gamma3)/TotalSum_3)
  alpha_3 <- 1 - sum(obsProb_3$V2)
  #------------------------------------------------------------
  obs_2 <- data.table(Ngram_Words[Ngram_Words$Prefix==Inp_1g[[1]] & 
                                     Ngram_Words$ngram==2 ,c(2,3)] %>%
                        arrange(desc(N)))
  TotalSum_2 <- sum(obs_2$N)
  obsProb_2 <- data.table(obs_2$Pred,(obs_2$N-gamma2)/TotalSum_2)
  alpha_2 <- 1 - sum(obsProb_2$V2)
  #------------------------------------------------------------
  TotalWords <- data.table(Ngram_Words[Ngram_Words$ngram==1,c(2,3)] %>% arrange(desc(N)))
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
  names(Prob_5) <- c("Prediction","Probability")
  Prob_5$Probability <- round(Prob_5$Probability ,3)
  data.table(top_n(Prob_5,5,Probability))
  
}



