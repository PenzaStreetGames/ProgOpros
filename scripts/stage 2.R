# Double tables
result$double_tables <- lapply(without_number(opros), function(x)
  lapply(without_number(opros), function(y) round(prop.table(table(x, y)), digits = 3)))
