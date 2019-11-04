for (i in 1:nrow(opros)) {
  for (j in 1:ncol(opros)) {
    if (is.na(opros[i, j]))
      print(paste(i, j))
  }
}
any(!complete.cases(opros))
