# Double tables
result$double_tables <- lapply(without_number(opros), function(x)
  lapply(without_number(opros), function(y) round(prop.table(table(x, y)), digits = 3)))

# Check equals
sapply(opros, function(x) 
  sum(sapply(opros, function(y) all(as.vector(x) == as.vector(y)))))
# all rigth

# Double Fisher Test
result$double_fisher_test <- lapply(opros[df_struct$quality_vars], function(x) 
  lapply(opros[df_struct$quality_vars][sapply(opros[df_struct$quality_vars], function(z) !all(as.vector(x) == as.vector(z)))], 
         function(y) fisher.test(table(x, y), simulate.p.value = T)))

# Cor test Kendall
result$cor_test_kendal <- lapply(opros[df_struct$numeric_vars], function(x) 
  lapply(opros[df_struct$numeric_vars]
         [sapply(opros[df_struct$numeric_vars], function(z) !all(as.vector(x) == as.vector(z)))],
         function(y) cor.test(x, y, method = "kendall")))

# Cor test Spearman
result$cor_test_spearman <- lapply(opros[df_struct$numeric_vars], function(x) 
  lapply(opros[df_struct$numeric_vars]
         [sapply(opros[df_struct$numeric_vars], function(z) !all(as.vector(x) == as.vector(z)))],
         function(y) cor.test(x, y, method = "spearman")))

# Kruskal test
result$kruskal_test <- lapply(opros[df_struct$numeric_vars], function(x)
  lapply(opros[df_struct$quality_vars], function(y)
    kruskal.test(x, y)))
