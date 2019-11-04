library(ggplot2)
for (i in 1:nrow(unique_fisher)) {
  x <- unique_fisher$x[i]
  y <- unique_fisher$y[i]
  name <- paste(x, "~", y)
  h <- ggplot(opros, aes(x = opros[[x]], fill = opros[[y]], group = opros[[y]], stat(count)))+
    geom_density(alpha = 0.3, position = "fill")+
    xlab(x)
  ggsave(file = paste0(getwd(), "/plots/density_plots/", i, ".png"), plot = h, device = "png")
}
ggplot(opros, aes(x = status, group = gender, fill = gender))+
  geom_density(alpha = 0.2)
