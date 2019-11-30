result$edged_vars_list <- data.frame(var = df_struct$vars, 
                                     edged_vars = sapply(opros, function(x) {
  x <- find_opros_col(x)
  paste(result$edged_vars[[x]], collapse = " ")
  }))
result$edged_vars_list
View(result$edged_vars_list)
rownames(result$edged_vars_list) <- NULL
colnames(result$edged_vars_list)
library(writexl)
write_xlsx(result$edged_vars_list, path = "edges_research.xlsx")

result$important_edges$all
write_xlsx(result$important_edges$all, path = "edges_list.xlsx")
