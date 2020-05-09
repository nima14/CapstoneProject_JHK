TrainTwitter <- read.table("TrainTwitter.txt",header = TRUE,fill=TRUE) %>% mutate(Source='Twitter')
 
TrainBlogs <- read.table("TrainBlogs.txt",header = TRUE,fill=TRUE)  %>% mutate(Source='Blogs')

TrainNews <- read.table("TrainNews.txt",header = TRUE,fill=TRUE)  %>% mutate(Source='News')

Train <- rbind(TrainTwitter,TrainBlogs,TrainNews)
rm(TrainTwitter,TrainBlogs,TrainNews)





