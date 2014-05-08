# load packages
require(bipartite)
require(reshape2)

# Set wd for data
setwd("~/Dropbox/Food Web Database/Mutualistic/web-of-life_2014-03-12_000716/")

whichFILES <- grep("M_PL", list.files())
plantPOL <- list()
for(i in 1:length(whichFILES)){
  plantPOL[[i]] <- read.csv(list.files()[i], header = T, row.names = 1)
}
names(plantPOL) <- list.files()[whichFILES]

# Convert matrices to edgelists with a weight column
elists <- lapply(plantPOL, function(x){
  df2 <- data.frame(plant = 0, pol = 0, weight = 0)
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      if(x[i,j] > 0){
        df <- data.frame(plant = colnames(x)[j], pol = rownames(x)[i], weight = x[i,j])
        df2 <- rbind(df2, df)
      }
    }
  }
  df2[2:nrow(df2),]
})

# Get the quantiles of the weight
quants <- t(sapply(elists, function(x){quantile(x[,3], probs = c(.5, .8, .9))}))
numInts <- melt(sapply(elists, nrow), value.name = "NumInt")

quantmat1 <- list()
quantmat2 <- list()
quantmat3 <- list()

for(i in 1:length(elists)){
  quantmat1[[i]] <- elists[[i]][elists[[i]]$weight > quants[i,1],]
  quantmat2[[i]] <- elists[[i]][elists[[i]]$weight > quants[i,2],]
  quantmat3[[i]] <- elists[[i]][elists[[i]]$weight > quants[i,3],]
}

mats <- lapply(quantmat3, acast, plant~pol)
mat <- lapply(mats, function(x){x[is.na(x)] <- 0})
lapply(quantmat3, nested, method = c("binmatnest", "NODF2"))
