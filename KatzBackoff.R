library(dplyr)
FF_OneG <- data.table( table(OneG_Words$N) )

FF_TwoG <- data.table( table(TwoG_Words$N) )
FF_TwoG$V1 <- as.numeric(FF_TwoG$V1)

FF_ThreeG <- data.table( table(ThreeG_Words$N) )

FF_FourG <- data.table( table(FourG_Words$N) )

FF_FiveG <- data.table( table(FiveG_Words$N) )


nls1=nls(N~i*V1^-z,start=list(i=300000,z=3),data=FF_TwoG)
FF_TwoG <-  data.table( FF_TwoG,New_N= as.integer(predict(nls1)) )


newdata <- data.frame(V1=1:max(FF_TwoG$V1+1)) 

pred <- data.table(newdata$V1,predict(nls1,newdata =newdata))




