unique_fisher <- data.frame(x = c(), y = c(), p = c())
for (i in 1:nrow(fisher_frame)) {
  relation <- fisher_frame[i,]
  relations <- paste(unique_fisher$x, unique_fisher$y)
  rev_relation <- paste(relation$y, relation$x)
  if (!(rev_relation %in% relations)) {
    unique_fisher <- rbind(unique_fisher, relation)
  }
}
nrow(fisher_frame)
paste(fisher_frame$x, fisher_frame$y)
sapply(fisher_frame, paste)
