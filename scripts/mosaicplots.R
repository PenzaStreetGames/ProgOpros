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
