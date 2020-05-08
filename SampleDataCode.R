library(quanteda)
library(dplyr)
library(ggplot2)
library(read)
library(stringi)
library(stringr)



setwd("N:/TempThings/final/en_US")

conTwitter <- file("en_US.twitter.txt", "r")
nTwitter <- length(count.fields(conTwitter, sep = ","))
close(conTwitter)

conNews <- file("en_US.news.txt", "r")
nNews <- length(count.fields(conNews, sep = ","))
close(conNews)

conBlogs <- file("en_US.blogs.txt", "r")
nBlogs <- length(count.fields(conBlogs, sep = ","))
close(conBlogs)



if (!file.exists("TrainTwitter.txt"))
{
setwd("N:/TempThings/final/en_US")


conTwitter <- file("en_US.twitter.txt", "r")
Twitter <- readLines(conTwitter, n=nTwitter, skipNul=TRUE, warn=FALSE)
close(conTwitter)


InLines <- which(rbinom(nTwitter,1,prob=0.1)==1)
SampleTwitter <- Twitter[InLines]

write.table(SampleTwitter, "TrainTwitter.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
}
#--------------------------------------------------------------------------------
setwd(Path)

if (!file.exists("TrainNews.txt"))
{
  
conNews <- file("en_US.news", "r")
News <- readLines(conNews, n=nNews, skipNul=TRUE, warn=FALSE)
close(conNews)


InLines <- which(rbinom(nNews,1,prob=0.1)==1)
SampleNews <- News[InLines]

write.table(SampleNews, "TrainNews.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
}
#---------------------------------------------------------
setwd(Path)

if (!file.exists("TrainBlogs.txt"))
{



InLines <- which(rbinom(nBlogs,1,prob=0.1)==1)
SampleBlogs <- Blogs[InLines]

write.table(SampleBlogs, "TrainBlogs.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
}

Path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(Path)