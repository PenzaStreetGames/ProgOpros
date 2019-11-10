# Researching edges
get_edges <- function(data, level) {
  df <- data.frame(x = c(), y = c(), p = c())
  lapply(names(data), function(x)
    lapply(names(data[[x]]), function(y) 
    { 
      if (data[[x]][[y]]$p.value < level) 
        if (!(paste(x, y) %in% c(apply(df, 1, function(x) paste(x["x"], x["y"])),
            apply(df, 1, function(x) paste(x["y"], x["x"])))))
          df <<- rbind(df, data.frame(x = x, y = y, p = data[[x]][[y]]$p.value)) 
    }
    ))
  df <- df[df$p < level,]
  df$p_log <- abs(round(log(df$p, base = 10)))
  df
}

level <- 0.05
result$important_edges$cor_test <- get_edges(result$cor_test_kendal, level)
result$important_edges$fisher_test <- get_edges(result$double_fisher_test, level)
result$important_edges$kruskal_test <- get_edges(result$kruskal_test, level)

# Uniting edges
result$important_edges$all <- rbind(result$important_edges$fisher_test, result$important_edges$cor_test,
      result$important_edges$kruskal_test)
