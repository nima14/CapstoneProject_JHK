library(dplyr)
FF_OneG <- data.table( table(OneG_Words$N) )

FF_TwoG <- data.table( table(TwoG_Words$N) )

FF_ThreeG <- data.table( table(ThreeG_Words$N) )

FF_FourG <- data.table( table(FourG_Words$N) )

FF_FiveG <- data.table( table(FiveG_Words$N) )


Bins <- FF_FiveG

Bins[2,V1]
max <- dim(Bins)[1]
r<-2:(max-1)
Bins[1, Zr:=2*N/Bins[2,V1]]  # r=1, q=0, Zr=Nr/(0.5t)
Bins[r, Zr:=2*Nr/(Bins[r+1,c]-Bins[r-1,c])]  # else, Zr=Nr/(0.5(t-q))
Bins[max, Zr:=Nr/(c-Bins[(max-1),c])]  # r=max, t=2r-q, Zr=Nr/(r-q)




Bins[64,2*N/(Bins[64+1,V1]-Bins[64-1,V1] )]

Bins[64+1,V1]

Bins[64-1,V1]


Bins[2*Bins$N/(Bins[4+1,V1]-Bins[4-1,V1])] 
