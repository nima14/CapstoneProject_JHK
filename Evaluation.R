library(dplyr)
library(stringi)
library(stringr)
library(data.table)
library(quanteda)
library(tm)

TrainTwitter <- read.table("TrainTwitter2.txt",header = TRUE,fill=TRUE) %>% mutate(Source='Twitter')
TrainBlogs <- read.table("TrainBlogs2.txt",header = TRUE,fill=TRUE)  %>% mutate(Source='Blogs')
TrainNews <- read.table("TrainNews2.txt",header = TRUE,fill=TRUE)  %>% mutate(Source='News')

TotalTrain <- rbind(TrainTwitter,TrainBlogs,TrainNews)

rm(TrainTwitter,TrainBlogs,TrainNews)


#Randomly shuffle the data
N <- nrow(TotalTrain)

set.seed(121)
TotalTrain<-TotalTrain[sample(N),]
#Create 10 equally size folds
folds <- cut(seq(1,N),breaks=10,labels=FALSE)
#Perform 10 fold cross validation

for(i in 1:10){
                    #Segement your data by fold using the which() function 
                    testIndexes <- which(folds==i,arr.ind=TRUE)
                    
                    Train<- TotalTrain[testIndexes, ]
                    
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
                    
                    OneG_Words <- data.table( table(as.character(Tok$x))  )
                  
                    
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
                    
                    Ngram_Words <- rbind(data.table(Prefix="",OneG_Words,ngram=1,Fold=i),
                                         data.table(TwoG_Words,ngram=2,Fold=i),
                                         data.table(ThreeG_Words,ngram=3,Fold=i),
                                         data.table(FourG_Words,ngram=4,Fold=i),
                                         data.table(FiveG_Words,ngram=5,Fold=i))
                    {
                    if (i==1) {Ngram_Words3 <- Ngram_Words}
                    else    {Ngram_Words3 <- rbind(Ngram_Words3,Ngram_Words)}
                    }
                    
                    Ngram_Words3 <-     Ngram_Words3 %>%filter(N>1) 
                    Ngram_Words3 <- data.table(Ngram_Words3)
                    setkey(Ngram_Words3,Fold,N)
    
                    
}


saveRDS(Ngram_Words3,"Ngram_Words3.Rdata")

Ngram_Words <- readRDS("Ngram_Words3.Rdata")
setkey(Ngram_Words,Fold)
rm(Base_FiveGram,Base_FourGram,Base_ThreeGram,Base_TwoGram,Corp,Fivegram,Fourgram
   ,Threegram,Twogram,Tok,FiveG_Words,FourG_Words,ThreeG_Words,TwoG_Words,OneG_Words
   ,Ngram_Words,Train,i,testIndexes)

K <- 10
ParGamma <- c(0.25,0.5,0.75)

GammaRes <- as.numeric( rep(0,K*length(ParGamma)^4))

for(gamma2 in ParGamma){
  for(gamma3 in ParGamma){
    for(gamma4 in ParGamma){
      for(gamma5 in ParGamma){
        
      }
    }
  }
}

for (i in 1:K) {

          
            Ngram_Words3 <-     Ngram_Words %>%filter(Fold!=i)  %>%
                  group_by(Prefix,Pred,ngram) %>% summarise(N=sum(N)) %>% data.table()
            setkey(Ngram_Words3,ngram,Prefix)
            
            testIndexes <- sample(which(folds==2,arr.ind=TRUE),1000,replace=FALSE)
            
            Test<- data.table(TotalTrain[testIndexes, ])
            
            Test<- data.table(iconv(Test$x, "UTF-8", "UTF-8",sub=''))
            Test$V1 <- tolower(Test$V1)
            Test$V1 <- gsub('[[:punct:] ]+',' ',Test$V1)
            Test$V1 <- gsub('[0-9]+', '', Test$V1)
            
            ProfanityWords <- read.table("Profanity.txt",sep="\n",quote = "")
            
            Test$V1 <- removeWords(Test$V1,ProfanityWords$V1)
            
            
               
               Test$len <- apply(Test,1,function(x) sapply(strsplit(x[[1]]," "),length))
               
               
   
               
               set.seed(124)
            Test$Start <- apply(Test,1,function(x) sample(1:(as.numeric(x[[2]])-1),1))
            
         
            
            Test$End <- apply(Test,1,function(x) 
              
                                
                              if( as.numeric(x[[3]])+3>as.numeric(x[[2]])-1)
                              {  return(as.numeric(x[[2]])-1) }
                              else { return(as.numeric(x[[3]])+3) }
                              )
                                                    

                                            
        Test$Prefix <- word(Test$V1,as.numeric(Test$Start),as.numeric(Test$End))
        
        Test$Pred <- word(Test$V1,as.numeric(Test$End)+1,as.numeric(Test$End)+1)
        
      
        Test$Prob <- apply(Test,1,function(x) GetObsProbs_Spec(x[[5]],x[[6]]))
        
        
        GammaIndx <-  {if(GammaRes[1]==0) 1
          else as.numeric(tail(which(GammaRes>0),1))+1}
        
        GammaRes[GammaIndx] <-sum(Test$Prob,na.rm = TRUE)/sum(complete.cases(Test))
        
        
        
}






