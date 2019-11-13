# Graph creating
net <- list()
net$edges <- data.frame(from = result$important_edges$all$x,
                        to = result$important_edges$all$y,
                        weight = result$important_edges$all$p_log)
net$vertices <- data.frame(
  id = colnames(opros),
  type = ifelse(colnames(opros) %in% df_struct$numeric_vars, "numeric", "quality"))
graph <- graph.data.frame(net$edges, net$vertices, directed = F)
V(graph)$color <- ifelse(net$vertices$id %in% df_struct$numeric_vars, "orange", "skyblue")
E(graph)$width <- as.numeric(as.vector(net$edges$weight))
l <- layout.kamada.kawai(graph)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
?plot
png(filename = "plots/graph.png", width = 1200, height = 900)
plot(graph, rescale=F, layout= l*4 , vertex.size = 15)
dev.off()
tkid <- tkplot(graph)
l <- tkplot.getcoords(tkid)
plot(graph, layout=l, vertex.size = 15)

library(igraph)
create_graph <- function(var) {
  t <- net$edges[apply(net$edges, 1, function(x) x["from"] == var | x["to"] == var),]
  t1 <- unique(c(as.vector(t$from), as.vector(t$to)))
  ifelse(t1 %in% df_struct$numeric_vars, "numeric", "quality")
  graph <- graph.data.frame(t, data.frame(
    id = t1,
    type = unique(c(as.vector(t$from), as.vector(t$to)))), directed = F)
  V(graph)$color <- ifelse(t1 %in% df_struct$numeric_vars, "orange", "skyblue")
  E(graph)$width <- as.numeric(as.vector(t$weight))
  plot(graph, layout=layout.fruchterman.reingold,
       vertex.size = 30)
}

sapply(df_struct$vars, function(x) {
  print(paste0("plots/dependence_graphs/", x, ".png"))
  png(filename = paste0("plots/dependence_graphs/", x, ".png"))
  create_graph(x)
  dev.off()
})
