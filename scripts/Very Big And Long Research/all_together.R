# rm(list = ls())
options(stringsAsFactors = TRUE)
library(readxl)
library(dplyr)
opros <- read_excel("ProgOprosEdited.xlsx")
opros <- select(opros, -number) # колонка номеров не нужна

# Функция, преобразующая набор строк в фактор (словарь)
factorise <- function(opros) {
  factoring_cols <-
    c(
      "gender",
      "status",
      "processor",
      "microboard",
      "desctop_os",
      "mobile_os",
      "editor_theme",
      "cycle_recursion",
      "cycle",
      "java_kotlin",
      "zero_division",
      "indexing",
      "typing",
      "slow_python",
      "list_mutable",
      "sugar",
      "list_expressions",
      "ternar_module",
      "patterns",
      "mobile_desctop",
      "web",
      "back_front_end",
      "flask_django",
      "python",
      "cpp",
      "javascript",
      "pascal",
      "csharp",
      "java",
      "c",
      "php",
      "kotlin",
      "lua",
      "scratch",
      "basic",
      "go",
      "ruby",
      "fasm",
      "bf",
      "haskel",
      "pycharm",
      "vscode",
      "idle",
      "notepad",
      "notepadpp",
      "wing",
      "sublime",
      "jupiter",
      "atom",
      "console",
      "machine_learning",
      "big_data",
      "metaprog",
      "quantum",
      "cryptography",
      "math"
    )
  colnames(opros)
  for (string in factoring_cols) {
    # print(string)
    opros[[string]] <- factor(opros[[string]])
  }
  
  return(opros)
}
df_struct <- list() # список, хранящий данные о структуре данных
df_struct$numeric_vars <- # список количественных переменных
  c(
    "languages_number",
    "editors_number",
    "future_number",
    "humour",
    "other_opinion",
    "sugar_using",
    "python_discontent",
    "middle_answers",
    "dont_know",
    "web_using",
    "apple"
  )
df_struct$quality_vars <- # список качественных переменных
  c(
    "gender",
    "status",
    "processor",
    "microboard",
    "desctop_os",
    "mobile_os",
    "editor_theme",
    "cycle_recursion",
    "cycle",
    "java_kotlin",
    "zero_division",
    "indexing",
    "typing",
    "slow_python",
    "list_mutable",
    "sugar",
    "list_expressions",
    "ternar_module",
    "patterns",
    "mobile_desctop",
    "web",
    "back_front_end",
    "flask_django",
    "python",
    "cpp",
    "javascript",
    "pascal",
    "csharp",
    "java",
    "c",
    "php",
    "kotlin",
    "lua",
    "scratch",
    "basic",
    "go",
    "ruby",
    "fasm",
    "bf",
    "haskel",
    "pycharm",
    "vscode",
    "idle",
    "notepad",
    "notepadpp",
    "wing",
    "sublime",
    "jupiter",
    "atom",
    "console",
    "machine_learning",
    "big_data",
    "metaprog",
    "quantum",
    "cryptography",
    "math"
  )
df_struct$vars <- colnames(opros) # список всех переменных
result <- list() # список для хранения промежуточных и итоговых результатов

opros <- factorise(opros) # факторизация всех сторковых столбцов
# str(opros)

## Стадия 1. Простая предобработка и гипотезы

# Simple tables
result$simple_tables <- list()
result$simple_tables <- apply(opros, 2, function(x) 
  round(prop.table(table(x)), digits = 3))

# Simple Fisher Test
result$simple_fisher_test <- apply(opros, 2, function(x)
  chisq.test(table(x)))

# Shapiro Test
result$shapiro_test <- lapply(opros[df_struct$numeric_vars], 
                              function(x) shapiro.test(x))
