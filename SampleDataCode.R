setwd("N:/TempThings/final/en_US")

conTwitter <- file("en_US.twitter.txt", "r")
nTwitter <- length(count.fields(conTwitter, sep = ","))
close(conTwitter)


conTwitter <- file("en_US.twitter.txt", "r")
Twitter <- readLines(conTwitter, n=nTwitter, skipNul=TRUE, warn=FALSE, encoding="UTF-8")
close(conTwitter)


InLines <- which(rbinom(nTwitter,1,prob=0.1)==1)
SampleTwitter <- Twitter[InLines]

write.table(SampleTwitter, "TrainTwitter.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
#--------------------------------------------------------------------------------

conNews <- file("en_US.news", "r")
nNews <- length(count.fields(conNews, sep = ","))
close(conNews)


conNews <- file("en_US.news", "r")
News <- readLines(conNews, n=nNews, skipNul=TRUE, warn=FALSE)
close(conNews)


InLines <- which(rbinom(nNews,1,prob=0.1)==1)
SampleNews <- News[InLines]

write.table(SampleNews, "TrainNews.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
#---------------------------------------------------------

conBlogs <- file("en_US.blogs.txt", "r")
nBlogs <- length(count.fields(conBlogs, sep = ","))
close(conBlogs)


conBlogs <- file("en_US.blogs.txt", "r")
Blogs <- readLines(conBlogs, n=nBlogs, skipNul=TRUE, warn=FALSE)
close(conBlogs)


InLines <- which(rbinom(nBlogs,1,prob=0.1)==1)
SampleBlogs <- Blogs[InLines]

write.table(SampleBlogs, "TrainBlogs.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

