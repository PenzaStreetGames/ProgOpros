for (i in c(quality_vars, numeric_vars)) {
  for (j in c(quality_vars, numeric_vars)) {
    if (i != j) {
      name <- paste(j, "~", i)
      t <- table(opros[[j]], opros[[i]])
      png(paste0(getwd(), "/plots/mosaicplots/", name, ".png"))
      mosaicplot(t, color = T, shade = T, main = name, xlab = j, ylab = i)
      dev.off()
    }
  }
}
mosaicplot(table(opros$gender, opros$languages_number), color = T, shade = T)
for (i in 1:nrow(unique_fisher)) {
  x <- unique_fisher$x[i]
  y <- unique_fisher$y[i]
  name <- paste(x, "~", y)
  t <- table(opros[[y]], opros[[x]])
  png(paste0(getwd(), "/plots/important_mosaicplots/", name, ".png"))
  mosaicplot(t, color = T, shade = T, main = name, xlab = y, ylab = x)
  dev.off()
}
