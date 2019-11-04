png(paste0(getwd(), "/plots/dendrograms/", "main", ".png"))
d <- dist(opros)
plot(hclust(d))
dev.off()
