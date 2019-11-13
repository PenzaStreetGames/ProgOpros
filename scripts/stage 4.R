x <- "gender"
str(result$important_edges$all)
result$edged_vars <- lapply(opros, function(x) {
  x <- names(opros)[sapply(names(opros), function(y) all(as.vector(opros[[y]]) == as.vector(x)))]
  r <- apply(result$important_edges$all[
    apply(result$important_edges$all, 1, function(y)
          y["x"] == x | y["y"] == x),], 1, function(y) {
    ifelse(y["x"] == x, y["y"], y["x"])
  })
  names(r) <- NULL
  r})

lapply(opros, function(x) {
  x <- names(opros)[sapply(names(opros), function(y) all(as.vector(opros[[y]]) == as.vector(x)))]
  if (length(result$edged_vars[[x]]) == 0)
    return("Нет значимых предикторов")
  if (x %in% df_struct$numeric_vars) {
    data <- opros[c(x, result$edged_vars[[x]])]
    fit <- lm(data[[1]] ~ data[[c(-1)]], data)
    perf_fit <- step(fit, direction = "backward")
    return(summary(perf_fit))
  }
})
library(lazyeval)
x <- "middle_answers"
data <- opros[c(x, result$edged_vars[[x]])]
fit <- lm(middle_answers ~ ., data)
perf_fit <- step(fit, direction = "backward")
result$regressions[[x]] <- summary(perf_fit)
result$regressions[[x]]

length(result$regressions)
