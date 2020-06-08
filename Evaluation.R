library(dplyr)
library(stringi)
library(stringr)
library(data.table)
library(quanteda)
library(tm)
library(doParallel)


TrainTwitter <- read.table("TrainTwitter.txt",header = TRUE,fill=TRUE) %>% mutate(Source='Twitter')
TrainBlogs <- read.table("TrainBlogs.txt",header = TRUE,fill=TRUE)  %>% mutate(Source='Blogs')
TrainNews <- read.table("TrainNews.txt",header = TRUE,fill=TRUE)  %>% mutate(Source='News')

TotalTrain <- rbind(TrainTwitter,TrainBlogs,TrainNews)

rm(TrainTwitter,TrainBlogs,TrainNews)

K <- 2
#Randomly shuffle the data
N <- nrow(TotalTrain)

set.seed(121)
TotalTrain<-TotalTrain[sample(N),]
#Create 10 equally size folds
folds <- cut(seq(1,N),breaks=K,labels=FALSE)
#Perform 10 fold cross validation

n_cores <- parallel::detectCores() - 2

cl <- makeCluster(n_cores)

registerDoParallel(cl)



Ngram_Words_Folds <- foreach(i = 1:K ,.combine=rbind,  .packages =c('data.table','stringi'
                                                               ,'stringr','dplyr',
                                                               'quanteda','tm')
                            ) %dopar% {
                          
                          
                    #Segement your data by fold using the which() function 
                    testIndexes <- which(folds==i,arr.ind=TRUE)
                    
                    Train<- TotalTrain[testIndexes, ]
                    
                    #Delete unkown characters.
                    Train<- data.table(iconv(Train$x, "UTF-8", "UTF-8",sub=''))
                    
                    
                    #Delete break lines.
                    Train <-  data.table(str_replace_all(Train$V1, "[\r\n]" , " "))
                    
                    
                    # Lowecase
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
                    
                    rm(Train)                   
                    #Create tokens
                    Tok <- data.table(tokens(Corp))
                    
                    rm(Corp)
                    
                    
                    # Building 2-gram & 3-grams
                    
                    Twogram <-  data.table(tokens_ngrams(Tok$V1, n =2, concatenator = " "))
                    
                    Threegram <-  data.table(tokens_ngrams(Tok$V1, n =3, concatenator = " "))
                    
                    Fourgram <-  data.table(tokens_ngrams(Tok$V1, n =4, concatenator = " "))
                    
                    Fivegram <-  data.table(tokens_ngrams(Tok$V1, n =5, concatenator = " "))
                    
                    
                    
                    ## Cleaning the Tokens
                    
                    OneG_Words <- data.table( table(as.character(Tok$V1))  )
                  
                    
                    Base_TwoGram <- data.table( table(as.character(Twogram$V1))  )
                    TwoG_Words <- data.table(str_split_fixed(Base_TwoGram$V1," ", 2),Base_TwoGram$N)
                    names(TwoG_Words) <- c("V1","V2","N")
                    
                    
                    Base_ThreeGram <- data.table( table(as.character(Threegram$V1))  )
                    ThreeG_Words <- data.table(str_split_fixed(Base_ThreeGram$V1," ", 3),Base_ThreeGram$N)
                    names(ThreeG_Words) <- c("V1","V2","V3","N")
                    
                    
                    Base_FourGram <- data.table( table(as.character(Fourgram$V1))  )
                    FourG_Words <- data.table(str_split_fixed(Base_FourGram$V1," ", 4),Base_FourGram$N)
                    names(FourG_Words) <- c("V1","V2","V3","V4","N")
                    
                    
                    
                    Base_FiveGram <- data.table( table(as.character(Fivegram$V1))  )
                    FiveG_Words <- data.table(str_split_fixed(Base_FiveGram$V1," ", 5),Base_FiveGram$N)
                    names(FiveG_Words) <- c("V1","V2","V3","V4","V5","N")
                    
                    
                    names(OneG_Words) <- c("Pred","N")
                    
                    
                    names(TwoG_Words) <- c("Prefix","Pred","N")
                    
                    
                    ThreeG_Words <- data.table(paste(ThreeG_Words$V1,ThreeG_Words$V2),ThreeG_Words$V3,ThreeG_Words$N)
                    names(ThreeG_Words) <- c("Prefix","Pred","N")
                    
                    
                    FourG_Words <- data.table(paste(FourG_Words$V1,FourG_Words$V2,FourG_Words$V3),FourG_Words$V4,FourG_Words$N)
                    names(FourG_Words) <- c("Prefix","Pred","N")
                    
                    
                    FiveG_Words <- data.table(paste(FiveG_Words$V1,FiveG_Words$V2,FiveG_Words$V3,FiveG_Words$V4),FiveG_Words$V5,FiveG_Words$N)
                    names(FiveG_Words) <- c("Prefix","Pred","N")
                    
                    Ngram_Words_Folds <- rbind(data.table(Prefix="",OneG_Words,ngram=1,Fold=i),
                                         data.table(TwoG_Words,ngram=2,Fold=i),
                                         data.table(ThreeG_Words,ngram=3,Fold=i),
                                         data.table(FourG_Words,ngram=4,Fold=i),
                                         data.table(FiveG_Words,ngram=5,Fold=i))
              
    
                    
}

stopCluster(cl)

Ngram_Words_Folds <-     Ngram_Words_Folds %>%filter(N>1) 
Ngram_Words_Folds <- data.table(Ngram_Words_Folds)



saveRDS(Ngram_Words_Folds,"Ngram_Words_Folds.Rdata")

  Ngram_Words_Folds <- readRDS("Ngram_Words_Folds.Rdata")
  setkey(Ngram_Words_Folds,Fold)
  



ParGamma <- c(0.1,0.2,0.5)

cl <- makeCluster(n_cores)
registerDoParallel(cl)
Res <-
  foreach(gamma2 = ParGamma, .combine = 'rbind') %:% 
  foreach(gamma3 = ParGamma, .combine = 'rbind')%:% 
  foreach(gamma4 = ParGamma, .combine = 'rbind')%:% 
  foreach(gamma5 = ParGamma, .combine = 'rbind')%:% 
  
 foreach (i = 1:K,.combine = 'rbind',.packages =c('data.table','stringi'
                                                   ,'stringr','dplyr',
                                                   'quanteda','tm')) %do% {
                                                     
                                                     setkey(Ngram_Words_Folds,Fold)
                                                     Ngram_Words_Folds_i <-     Ngram_Words_Folds %>%filter(Fold!=i)  %>%
                                                       group_by(Prefix,Pred,ngram) %>% summarise(N=sum(N)) %>% data.table()
                                                     setkey(Ngram_Words_Folds_i,ngram,Prefix)
                                                     
                                                     # set.seed(124)
                                                     testIndexes <- top_n(as.data.frame(which(folds==i,arr.ind=TRUE)),200)[[1]]
                                                     
                                                     Test<- data.table(TotalTrain[testIndexes, ])
                                                     
                                                     Test<- data.table(iconv(Test$x, "UTF-8", "UTF-8",sub=''))
                                                     Test$V1 <- tolower(Test$V1)
                                                     Test$V1 <- gsub('[[:punct:] ]+',' ',Test$V1)
                                                     Test$V1 <- gsub('[0-9]+', '', Test$V1)
                                                     
                                                     ProfanityWords <- read.table("Profanity.txt",sep="\n",quote = "")
                                                     
                                                     Test$V1 <- removeWords(Test$V1,ProfanityWords$V1)
                                                     
                                                     
                                                     
                                                     Test$len <- apply(Test,1,function(x) sapply(strsplit(x[[1]]," "),length))
                                                     
                                                     
                                                     
                                                     
                                                     set.seed(125)
                                                     Test$Start <- apply(Test,1,function(x) sample(1:(as.numeric(x[[2]])-1),1))
                                                     
                                                     
                                                     
                                                     Test$End <- apply(Test,1,function(x) 
                                                       
                                                       
                                                       if( as.numeric(x[[3]])+3>as.numeric(x[[2]])-1)
                                                       {  return(as.numeric(x[[2]])-1) }
                                                       else { return(as.numeric(x[[3]])+3) }
                                                     )
                                                     
                                                     
                                                     
                                                     Test$Prefix <- word(Test$V1,as.numeric(Test$Start),as.numeric(Test$End))
                                                     
                                                     Test$Pred <- word(Test$V1,as.numeric(Test$End)+1,as.numeric(Test$End)+1)
                                                     
                                                     
                                                     Test$Prob <- apply(Test,1,function(x) GetObsProbs_Spec(x[[5]],x[[6]],
                                                                                                            gamma2,gamma3,gamma4,gamma5,
                                                                                                            Ngram_Words_Folds_i))
                                                     
                                                     
                                                     Result <- c(sum(Test$Prob,na.rm = TRUE)/sum(!is.na(Test$Prob)),gamma2,gamma3
                                                                 ,gamma4,gamma5,i)
                                                     names(Result) <- c("Prob","gamma2","gamma3","gamma4","gamma5","Folds")
                                                     Result
                                                     
                                                     
                                                     
                                                   }

stopCluster(cl)


saveRDS(Res,"Res3.Rdata")


Res <- readRDS("Res3.Rdata")


Res <- Res %>% data.table() %>% group_by(gamma2,gamma3,gamma4,gamma5) %>% summarise(mean(Prob)) %>% data.table()

names(Res) <- c("gamma2","gamma3","gamma4","gamma5","Prob")


Res.Max <- Res[which.max(Res$Prob)]

Res.Max


Res %>% arrange(desc(Prob))


top_n(Res,10,Prob)
