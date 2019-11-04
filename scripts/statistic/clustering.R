smart_hclust <- function(test_data, n_cluster){    
  d <- dist(test_data)    
  fit <- hclust(d)    
  test_data$cluster <- factor(cutree(fit, k = n_cluster))    
  return(test_data)    
}
d <- dist(opros)
plot(hclust(d))
opros_c4 <- smart_hclust(opros, 4)
opros_c4$cluster <- factor(opros_c4$cluster, labels = c("red", "green", "blue", "purple"))
table(opros_c4$cluster)
