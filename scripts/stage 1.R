# Simple tables
result$simple_tables <- list()
result$simple_tables <- apply(opros, 2, function(x) 
  round(prop.table(table(x)), digits = 3))
for (var in colnames(opros)[2:ncol(opros)]) {
  result$simple_tables <- apply(table(opros[[var]]))
}

# Simple Fisher Test
result$simple_fisher_test <- apply(opros, 2, function(x)
  chisq.test(table(x)))

# Shapiro Test
result$shapiro_test <- lapply(opros[df_struct$numeric_vars], 
                              function(x) shapiro.test(x))
