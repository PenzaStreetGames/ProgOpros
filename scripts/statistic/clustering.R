smart_hclust <- function(test_data, n_cluster){    
  d <- dist(test_data[2:ncol(test_data)])    
  fit <- hclust(d)    
  test_data$cluster <- factor(cutree(fit, k = n_cluster))    
  return(test_data)    
}
opros[4:14]
d <- dist(opros[2:ncol(opros)])
plot(hclust(d))
opros_c5 <- smart_hclust(opros, 5)
opros_c5$cluster <- factor(opros_c5$cluster)
table(opros_c5$cluster, opros_c5$humour)
subset_c4_red <- opros[opros_c4$cluster == "red",]
subset_c4_green <- opros[opros_c4$cluster == "green",]
subset_c4_blue <- opros[opros_c4$cluster == "blue",]
subset_c4_purple <- opros[opros_c4$cluster == "purple",]
rm("subset_c4_1", "subset_c4_2", "subset_c4_3", "subset_c4_4")
