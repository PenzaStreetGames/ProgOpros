library(ggplot2)
jitter_range <- 0.4
for (i in 1:nrow(unique_fisher)) {
  x <- unique_fisher$x[i]
  y <- unique_fisher$y[i]
  name <- paste(x, "~", y, "clustered")
  p <- ggplot(opros_c4, aes(x = opros[[x]], y = opros[[y]], color = cluster, size = 5, alpha = 0.95))+
    geom_point()+
    geom_jitter(width = jitter_range, height = jitter_range)+
    xlab(x)+
    ylab(y)
  ggsave(file = paste0(getwd(), "/plots/important_pointplots/", name, ".png"), plot = p, device = "png")
}
ggplot(opros_c4, aes(x = opros[[x]], y = opros[[y]], color = cluster, size = 5, alpha = 0.95))+
  geom_point()+
  geom_jitter(width = jitter_range, height = jitter_range)+
  xlab(x)+
  ylab(y)
