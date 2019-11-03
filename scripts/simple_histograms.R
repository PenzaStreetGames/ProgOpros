library(ggplot2)
for (i in quality_vars) {
  data = opros[[i]]
  h <- ggplot(opros, aes(x = data, fill = data))+
    geom_histogram(stat = "count")+
    xlab(i)
  ggsave(file = paste0(getwd(), "/plots/simple_histograms/", i, ".png"), plot = h, device = "png")
}
for (i in numeric_vars) {
  data = opros[[i]]
  h <- ggplot(opros, aes(x = data, fill = "red"))+
    geom_density()+
    xlab(i)
  ggsave(file = paste0(getwd(), "/plots/simple_density/", i, ".png"), plot = h, device = "png")
}
