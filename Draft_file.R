Base_FiveGram[match(x[4],word(Base_FiveGram$V1,1,4))]
Base_FourGram[match(x[3],word(Base_FourGram$V1,1,3))]
Base_ThreeGram[match(x[2],word(Base_ThreeGram$V1,1,2))]
Base_TwoGram[match(x[1],word(Base_TwoGram$V1,1,1))]


system.time(
  Base_ThreeGram[match(x[2],word(Base_ThreeGram$V1,1,2))]
)



saveRDS(Base_FiveGram,"Base_FiveGram.Rdata")

Base_FiveGram <- readRDS("Base_FiveGram.Rdata")