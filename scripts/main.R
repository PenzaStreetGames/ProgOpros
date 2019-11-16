rm(numeric_vars, quality_vars)
options(stringsAsFactors = F)
library(readxl)
opros <- read_excel("ProgOprosEdited.xlsx")
opros <- factorise(opros)
df_struct <- list()
df_struct$numeric_vars <- numeric_vars
df_struct$quality_vars <- quality_vars
df_struct$vars <- colnames(opros)
result <- list()
save(result, opros, df_struct, net, file = "result.RData")
numered_opros <- opros
opros$number <- NULL

without_number <- function(opros) {
  opros[,2:ncol(opros)]
}

# NAs.R
# wide_vars_split.R
# summary_vars.R
# sythetic_vars.R
# factorise.R

# temp_save.R

# chisq_and_fisher_tests.R
# replaces_deletions.R

library(ggplot2)
qplot(x = mobile_os, data = opros, fill = gender, color = I("black"))

hclust(opros[[c("gender", "status")]])

dist_opros <- dist(opros)
model <- hclust(dist_opros)
plot(model)
