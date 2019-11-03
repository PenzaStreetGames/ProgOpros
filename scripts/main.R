rm(list = ls())
options(stringsAsFactors = F)
library(readxl)
opros <- read_excel("OprosProg.xlsx")

# NAs.R
# wide_vars_split.R
# summary_vars.R
# sythetic_vars.R
# factorise.R

# temp_save.R

# chisq_and_fisher_tests.R
# replaces_deletions.R

hclust(opros[[c("gender", "status")]])

dist_opros <- dist(opros)
model <- hclust(dist_opros)
plot(model)
