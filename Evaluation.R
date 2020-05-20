library(caret)


TrainTwitter <- read.table("TrainTwitter2.txt",header = TRUE,fill=TRUE) %>% mutate(Source='Twitter')
TrainBlogs <- read.table("TrainBlogs2.txt",header = TRUE,fill=TRUE)  %>% mutate(Source='Blogs')
TrainNews <- read.table("TrainNews2.txt",header = TRUE,fill=TRUE)  %>% mutate(Source='News')

Train <- rbind(TrainTwitter,TrainBlogs,TrainNews)

rm(TrainTwitter,TrainBlogs,TrainNews)

Corp <- data.table(corpus(as.character(Train$x)))

flds <- createFolds(Corp$V1, k = 10, list = TRUE, returnTrain = FALSE)
names(flds)[1] <- "train"