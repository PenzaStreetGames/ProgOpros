find_opros_col <- function(x) {
  names(opros)[sapply(names(opros), function(y) all(as.vector(opros[[y]]) == as.vector(x)))]
}

str(result$important_edges$all)
result$edged_vars <- lapply(opros, function(x) {
  x <- find_opros_col(x)
  r <- apply(result$important_edges$all[
    apply(result$important_edges$all, 1, function(y)
          y["x"] == x | y["y"] == x),], 1, function(y) {
    ifelse(y["x"] == x, y["y"], y["x"])
  })
  names(r) <- NULL
  r})
result$edged_vars

result$linear_regressions <- lapply(opros[,df_struct$numeric_vars], function(x) {
  x <- find_opros_col(x)
  data <- opros[result$edged_vars[[x]]]
  fit <- lm(opros[[x]] ~ ., data)
  perf_fit <- step(fit, direction = "backward")
  summary(perf_fit)
})

