# Graph creating
net <- list()
net$edges <- data.frame(from = result$important_edges$all$x,
                        to = result$important_edges$all$y,
                        weight = result$important_edges$all$p_log)
net$vertices <- data.frame(
  id = colnames(opros),
  type = ifelse(colnames(opros) %in% df_struct$numeric_vars, "numeric", "quality"))

library(igraph)
graph <- graph.data.frame(net$edges, net$vertices, directed = F)
graph
l <- layout.kamada.kawai(graph)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
V(graph)$color <- sapply(factor(net$vertices$type), function(x) c("orange", "skyblue")[x])
E(graph)$width <- net$edges$weight
plot(graph, edge.arrow.size=0.4, rescale = F, layout=l)
