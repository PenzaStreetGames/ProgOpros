options(stringsAsFactors = F)
print("Chisq test:")
chisq_frame <- data.frame(x = c(), y = c(), p = c())
for (i in colnames(opros)) {
  for (j in colnames(opros)) {
    if (i != j) {
      test <- chisq.test(table(opros[[i]], opros[[j]]))
      if (test$p.value <= 0.05)
        chisq_frame <- rbind.data.frame(chisq_frame, data.frame(x = c(i), y = c(j), p = test$p.value))
        print(paste(i, j, test$p.value))
    }
  }
}
chisq_frame <- chisq_frame[order(chisq_frame$p),]
print("Fisher test:")
fisher_frame <- data.frame(x = c(), y = c(), p = c())
for (i in colnames(opros)) {
  for (j in colnames(opros)) {
    if (i != j) {
      test <- fisher.test(opros[[i]], opros[[j]], simulate.p.value = T)
      if (test$p.value <= 0.05)
        fisher_frame <- rbind.data.frame(fisher_frame, data.frame(x = c(i), y = c(j), p = test$p.value))
        print(paste(i, j, test$p.value))
    }
  }
}
fisher_frame <- fisher_frame[order(fisher_frame$p),]
