j <- "gender"
for (i in quality_vars) {
  name <- paste(j, "~", i)
  t <- table(opros[[j]], opros[[i]])
  png(paste0(getwd(), "/plots/mosaicplots/", name, ".png"))
  mosaicplot(t, color = T, shade = T, main = name)
  dev.off()
}
