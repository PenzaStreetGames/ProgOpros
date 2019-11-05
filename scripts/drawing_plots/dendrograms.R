png(paste0(getwd(), "/plots/dendrograms/", "main1", ".png"))
d <- dist(opros[-c(1)])
plot(hclust(d))
dev.off()
