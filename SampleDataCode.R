setwd("C:/Users/n.taghidoost/Downloads/Compressed/final/en_US")
con <- file("en_US.twitter.txt", "r")

n <- length(count.fields(con, sep = ","))

close(con)


con <- file("en_US.twitter.txt", "r")

InLines <- which(rbinom(n,1,prob=0.1)==1)

x <- read.table(con, sep="\n", header=FALSE , quote="" )

SampleFile <- x[InLines,]

write.table(SampleFile, "TrainFile.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)