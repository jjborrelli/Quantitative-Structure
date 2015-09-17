data <- read.csv("C:/Users/jjborrelli/Dropbox/Food Web Database/Food_Web/Assembly/ele12264-sup-0005-Masterdata.csv")
head(data)
data$period
data$distance
data$inoculated
data$pool
split1 <- split(data, data$pool)
length(split1)

require(igraph)

test1 <- split1[[1]][split1[[1]]$period == 1,]
edge1 <- matrix(c(as.character(test1$prey_ID), as.character(test1$pred_ID)), nrow = 5, ncol = 2)
plot.igraph(graph.edgelist(edge1))

test2 <- split1[[1]][split1[[1]]$period == 2,]
edge2 <- matrix(c(as.character(test2$prey_ID), as.character(test2$pred_ID)), nrow = 4, ncol = 2)
plot.igraph(graph.edgelist(edge2))

test3 <- split1[[1]][split1[[1]]$period == 3,]
edge3 <- matrix(c(as.character(test3$prey_ID), as.character(test3$pred_ID)), nrow = 10, ncol = 2)
plot.igraph(graph.edgelist(edge3))

test4 <- split1[[1]][split1[[1]]$period == 4,]
edge4 <- matrix(c(as.character(test4$prey_ID), as.character(test4$pred_ID)), nrow = 15, ncol = 2)
plot.igraph(graph.edgelist(edge4))


source("https://raw.githubusercontent.com/jjborrelli/Ecological-Networks/master/FoodWebs/Rscripts/web_functions.R")
motif_counter(list(graph.edgelist(edge4), graph.edgelist(edge3), graph.edgelist(edge2), graph.edgelist(edge1)), 4:1)


##### -----------------------------------------------------------------------------


library(igraph)
library(data.table)
source("https://raw.githubusercontent.com/jjborrelli/Ecological-Networks/master/FoodWebs/Rscripts/web_functions.R")

sp.data <- split(data, data$inoculated)[[1]]
sp.data.in <- split(data, data$inoculated)[[2]]

sp.data <- split(sp.data, f = sp.data$lake, drop = T)
sp.data.in <- split(sp.data.in, f = sp.data.in$lake, drop = T)

sp.data <- unlist(lapply(sp.data, function(x){split(x, f = x$pool, drop = T)}), recursive = F)
sp.data.in <- unlist(lapply(sp.data.in, function(x){split(x, f = x$pool, drop = T)}), recursive = F)

web.series <- function(df, motifs = T){
  s1 <- split(df, f = df$period)
  edge.l <- lapply(s1, function(x){matrix(c(as.character(x$prey_ID), as.character(x$pred_ID)), nrow = nrow(x), ncol = 2)})
  g.l <- lapply(edge.l, graph.edgelist)
  
  if(motifs){
    m <- motif_counter(g.l, webs = paste(names(s1),sapply(s1, function(x){as.character(x$pool[1])}), sep = "-"))
    return(m)
  }else{return(g.l)}
}

mots <- lapply(sp.data, web.series, motifs = T)
mots.i <- lapply(sp.data.in, web.series, motifs = T)
mots <- rbindlist(mots)
mots.i <- rbindlist(mots.i)

web.info <- function(df){
  s1 <- split(df, f = df$period)
  edge.l <- lapply(s1, function(x){matrix(c(as.character(x$prey_ID), as.character(x$pred_ID)), nrow = nrow(x), ncol = 2)})
  g.l <- lapply(edge.l, graph.edgelist)
  
  fwi <- get_fw_indices(lapply(g.l, get.adjacency, sparse = F), g.l, paste(names(s1),sapply(s1, function(x){as.character(x$pool[1])}), sep = "-"))
  return(cbind(period = names(s1), web = sapply(s1, function(x){as.character(x$pool[1])}), fwi))
}

fwd <- lapply(sp.data, web.info)
webd <- rbindlist(fwd)
ggplot(melt(webd), aes(x = period, y = value)) + geom_boxplot(aes(middle = "mean")) + facet_wrap(~variable, scale = "free_y")
