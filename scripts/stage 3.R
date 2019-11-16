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

# Removing obvious edges in Kruskal test
remove_obvious_edges <- function(data) {
  obvious_edges <- list(
    humour = c("gender", "status", "editor_theme"),
    other_opinion = c("zero_division", "indexing", "typing"),
    python_discontent = c("slow_python", "list_mutable", "sugar"),
    sugar_using = c("list_expressions", "ternar_module", "patterns"),
    apple = c("mobile_os", "desctop_os"),
    dont_know = c("microboard", "list_expressions", "ternar_module", "patterns", "flask_django"),
    web_using = c("web", "flask_django"),
    middle_answers = c("processor", "desctop_os", "mobile_os", "cycle_recursion", 
                       "cycle", "java_kotlin", "slow_python", "list_mutable", "sugar",
                       "mobile_desctop", "flask_django"),
    languages_number = c("python", "cpp",
                        "javascript", "pascal", "csharp", "java", "c",
                        "php", "kotlin", "lua", "scratch", "basic",
                        "go", "ruby", "fasm", "bf", "haskel"),
    editors_number = c("pycharm", "vscode", "idle", "notepad",
                       "notepadpp", "wing", "sublime", "jupiter",
                       "atom", "console"),
    future_number = c("machine_learning", "big_data", "metaprog",
                      "quantum", "cryptography", "math")
  )
  answer <- data.frame(x = c(), y = c(), p = c(), p_log = c())
  apply(result$important_edges$kruskal_test, 1, function(x) {
    if (!(x[["x"]] %in% names(obvious_edges) & 
          any(sapply(unlist(obvious_edges[as.vector(x[["x"]])]), function(z) as.vector(x[["y"]]) == z)))) {
      answer <<- rbind(answer, data.frame(x = c(x["x"]), y = c(x["y"]), 
                                            p = c(x["p"]), p_log = c(x["p_log"])))
    }
  })
  row.names(answer) <- NULL
  answer
}

get_all_edges <- function(level) {
  result$important_edges$cor_test <<- get_edges(result$cor_test_kendal, level)
  result$important_edges$fisher_test <<- get_edges(result$double_fisher_test, level)
  result$important_edges$kruskal_test <<- get_edges(result$kruskal_test, level)
  result$important_edges$kruskal_test <<- remove_obvious_edges(result$important_edges$kruskal_test)
  
  # Uniting edges
  result$important_edges$all <<- rbind(result$important_edges$fisher_test, result$important_edges$cor_test,
        result$important_edges$kruskal_test)
  result$important_edges$all$x <<- as.character(result$important_edges$all$x)
  result$important_edges$all$y <<- as.character(result$important_edges$all$y)
  result$important_edges$all$p <<- as.numeric(result$important_edges$all$p)
  result$important_edges$all$p_log <<- as.numeric(result$important_edges$all$p_log)
  result$important_edges$all
}
get_all_edges(0.05)
