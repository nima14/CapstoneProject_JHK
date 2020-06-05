setwd("C:/Users/n.taghidoost/Downloads/Compressed/final/en_US")
blogs<-file("en_US.blogs.txt","r")
blogs_lines<-readLines(blogs)
close(blogs)

summary(nchar(blogs_lines))


twitter<-file("en_US.twitter.txt","r")
twitter_lines<-readLines(twitter)
close(twitter)

sum(grepl("love",twitter_lines)) / sum(grepl("hate",twitter_lines))


twitter_lines[grep( "^A computer once beat me at chess, but it was no match for me at kickboxing$",twitter_lines)]

