data <- read.csv("~/Dropbox/Food Web Database/Food_Web/Assembly/ele12264-sup-0005-Masterdata.csv")
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


source("~/Desktop/GitHub/Ecological-Networks/FoodWebs/Rscripts/web_functions.R")
motif_counter(list(graph.edgelist(edge4), graph.edgelist(edge3), graph.edgelist(edge2), graph.edgelist(edge1)), 4:1)
