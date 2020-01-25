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
df_struct$simple_vars <- # список логически полных переменных
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
df_struct$complex_vars <- # список сгруппированных переменных
  list(
    languages = c(
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
      "haskel"
    ),
    editors = c(
      "pycharm",
      "vscode",
      "idle",
      "notepad",
      "notepadpp",
      "wing",
      "sublime",
      "jupiter",
      "atom",
      "console"
    ),
    futures = c(
      "machine_learning",
      "big_data",
      "metaprog",
      "quantum",
      "cryptography",
      "math"
    )
  )
result <- list() # список для хранения промежуточных и итоговых результатов

opros <- factorise(opros) # факторизация всех сторковых столбцов
# str(opros)

## Стадия 1. Простая предобработка и гипотезы

# Simple tables
result$simple_tables <- apply(opros, 2, function(x) 
  round(prop.table(table(x)), digits = 3))

# Simple Fisher Test
result$simple_fisher_test <- apply(opros, 2, function(x)
  chisq.test(table(x)))

# Shapiro Test
result$shapiro_test <- lapply(opros[df_struct$numeric_vars], 
                              function(x) shapiro.test(x))

## Стадия 2. Двойные таблицы и взаимосвязи

# Double tables
result$double_tables <- lapply(opros, function(x)
  lapply(opros, function(y) round(prop.table(table(x, y)), digits = 3)))

# Check equals
# sapply(opros, function(x) 
#  sum(sapply(opros, function(y) all(as.vector(x) == as.vector(y)))))
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

# Kruskal test
result$kruskal_test <- lapply(opros[df_struct$numeric_vars], function(x)
  lapply(opros[df_struct$quality_vars], function(y)
    kruskal.test(x, y)))

## Исследование взаимосвязей

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
result$important_edges$all <- get_all_edges(0.05)

find_opros_col <- function(x) {
  names(opros)[sapply(names(opros), function(y) all(as.vector(opros[[y]]) == as.vector(x)))]
}

# str(result$important_edges$all)
result$edged_vars <- lapply(opros, function(x) {
  x <- find_opros_col(x)
  r <- apply(result$important_edges$all[
    apply(result$important_edges$all, 1, function(y)
      y["x"] == x | y["y"] == x),], 1, function(y) {
        ifelse(y["x"] == x, y["y"], y["x"])
      })
  names(r) <- NULL
  r})
# result$edged_vars

result$edged_vars_list <- data.frame(var = df_struct$vars, 
                                     edged_vars = sapply(opros, function(x) {
                                       x <- find_opros_col(x)
                                       paste(result$edged_vars[[x]], collapse = " ")
                                     }))

var_labels <- c(gender = "Пол",
                status = "Социальный\nстатус",
                languages_number = "Количество используемых языков",
                editors_number = "Количество используемых редакторов",   
                future_number = "Количество выбранных перспективных отраслей",
                humour = "Коэффциент юмора",
                other_opinion = "Коэффициент другого мнения",
                python_discontent = "Коэффициент недовольства питоном",
                sugar_using = "Коэффициент использования синтаксического сахара",
                middle_answers = "Коэффициент лояльности ответов",
                dont_know = "Коэффициент незнания",
                web_using = "Коэффициент использования web'а",     
                apple = "Коэффициент лояльности к Apple",
                processor = "Выбор процессора",
                microboard = "Выбор микроплаты",
                desctop_os = "Выбор\nнастольной ОС",
                mobile_os = "Выбор\nмобильной ОС",
                editor_theme = "Выбор\nтемы редактора",
                cycle_recursion = "Цикл\nили\nрекурсия",
                cycle = "Выбор\nвида цикла",
                java_kotlin = "Java\nили\nKotlin",
                zero_division = "Мнение\nо делении\nна ноль",
                indexing = "Выбор индексации",
                typing = "Сколько будет\n1 + \"а\"",           
                slow_python = "Мнение\nо скорости\nпитона",
                list_mutable = "Отношение\nк изменяемости\nсписков в питоне",
                sugar = "Мнение\nо \"сахарности\"\nпитона",
                list_expressions = "Использование\nсписочных\nвыражений",
                ternar_module = "Использование\nтернарного\nмодуля",
                patterns = "Использование\nшаблонов\nпроектирования",
                mobile_desctop = "Мобильная\nразработка\nили\nдесктоп",
                web = "Необходимость\nвеба",           
                back_front_end = "Предпочитаемая\nотрасль\nвеба",
                flask_django = "Flask\nили\nDjango",
                python = "Использование\nPython",
                cpp = "Использование\nC++",        
                javascript = "Использование\nJavaScript",
                pascal = "Использование\nPascal",
                csharp = "Использование\nC#",
                java = "Использование\nJava",
                c = "Использование\nC",
                php = "Использование\nPHP",
                kotlin = "Использование\nKotlin",
                lua = "Использование\nLua",      
                scratch = "Использование\nScratch",
                basic = "Использование\nBasic",
                go = "Использование\nGo",
                ruby = "Использование\nRuby",
                fasm = "Использование\nFASM",
                bf = "Использование\nBrainfuck",
                haskel = "Использование\nHaskel",
                pycharm = "Использование\nPyCharm",
                vscode = "Использование\nVisual Studio\nCode",
                idle = "Использование\nIDLE",
                notepad = "Использование\nБлокнота",
                notepadpp = "Использование\nNotepad++",
                wing = "Использование\nWing",
                sublime = "Использование\nSublime Text",
                jupiter = "Использование\nJupiter\nNotebook",
                atom = "Использование\nAtom", 
                console = "Использование\nконсоли",
                machine_learning = "Ожидание\nразвития\nмашинного\nобучения",
                big_data = "Ожидание\nразвития\nбольших\nданных",
                metaprog = "Ожидание\nразвития\nметапрограммирования",        
                quantum = "Ожидание\nразвития\nквантовой\nлогики",
                cryptography = "Ожидание\nразвития\nкриптографии",
                math = "Ожидание\nразвития\nприкладной\nматематики")
spaced_var_labels <- sapply(var_labels, function(x) gsub("\n", " ", x))
full_var_labels <- c(
  python = "01. Python",
  cpp = "02. C++",        
  javascript = "03. JavaScript",
  pascal = "04. Pascal",
  csharp = "05. C#",
  java = "06. Java",
  c = "07. C",
  php = "08. PHP",
  kotlin = "09. Kotlin",
  lua = "10. Lua",      
  scratch = "11. Scratch",
  basic = "12. Basic",
  go = "13. Go",
  ruby = "14. Ruby",
  fasm = "15. FASM",
  bf = "16. Brainfuck",
  haskel = "17. Haskel",
  pycharm = "01. PyCharm",
  vscode = "02. Visual Studio\nCode",
  idle = "03. IDLE",
  notepad = "04. Блокнот",
  notepadpp = "05. Notepad++",
  wing = "06. Wing",
  sublime = "07. Sublime\nText",
  jupiter = "08. Jupiter\nNotebook",
  atom = "09. Atom", 
  console = "10. Консоль",
  machine_learning = "01. Машинное\nобучение",
  big_data = "02. Большие\nданные",
  metaprog = "03. Метапрограммирование",        
  quantum = "04. Квантовая\nлогика",
  cryptography = "05. Криптография",
  math = "06. Прикладная\nматематика"
)
var_positive_flags <- c(
  languages = "Использует",
  editors = "Использует",
  futures = "Ожидает"
)
complex_var_labels <- c(
  languages = "Популярность\nязыков\nпрограммирования",
  editors = "Популярность\nредакторов\nкода",
  futures = "Популярность\nвозможных\nпередовых\nотраслей"
)
spaced_complex_var_labels <- sapply(complex_var_labels, function(x) gsub("\n", " ", x))

## Отрисовка графиков

library(ggplot2)
library(extrafont)
loadfonts(device = "win")

theme_pablo <- theme(panel.background = element_rect(fill = "grey70"), 
                     plot.background = element_rect(fill = "grey60"),
                     legend.background = element_rect(fill = "grey80"),
                     text = element_text(family = "Comic Sans MS"),
                     axis.text.x = element_text(angle = 30, hjust = 1))

draw_plot <- function(x, y) {
  if (x %in% df_struct$numeric_vars & y %in% df_struct$numeric_vars) {
    random_colors <- colors()[sample(1:length(colors()), size = 2, replace = F)]
    plt <- ggplot(opros, aes(x = opros[[x]], y = opros[[y]], 
                             size = I(5), color = I(random_colors[1])))+
      geom_smooth(method = "lm", se = F, color = I(random_colors[2]), size = I(3))+
      geom_point()+
      scale_x_continuous(breaks = seq(1:10))+
      geom_jitter()+
      xlab(spaced_var_labels[x])+
      ylab(spaced_var_labels[y])+
      ggtitle(paste("Взаимосвязь переменных\n", spaced_var_labels[x], "и",
                    spaced_var_labels[y]))+
      theme_pablo
  }
  else if (x %in% df_struct$numeric_vars & y %in% df_struct$quality_vars) {
    plt <- ggplot(opros, aes(x = opros[[x]], fill = opros[[y]], color = I("black")))+
      geom_histogram(alpha = 0.8, binwidth = 1)+
      facet_wrap(~ opros[[y]], nrow = 3)+
      xlab(spaced_var_labels[x])+
      ylab("Частота")+
      ggtitle(paste("Взаимосвязь переменных\n", spaced_var_labels[x], "и",
                    spaced_var_labels[y]))+
      scale_fill_brewer(name = var_labels[y],
                        type = "qual", palette = sample(1:8, size=1))+
      scale_x_continuous(breaks = seq(1:10))+
      theme_pablo
  }
  else if (x %in% df_struct$quality_vars & y %in% df_struct$numeric_vars) {
    plt <- ggplot(opros, aes(x = opros[[y]], fill = opros[[x]], color = I("black")))+
      geom_histogram(alpha = 0.8, binwidth = 1)+
      facet_wrap(~ opros[[x]], nrow = 3)+
      xlab(spaced_var_labels[y])+
      ylab("Количество")+
      ggtitle(paste("Взаимосвязь переменных\n", spaced_var_labels[x], "и",
                    spaced_var_labels[y]))+
      scale_fill_brewer(name = var_labels[x],
                        type = "qual", palette = sample(1:8, size=1))+
      scale_x_continuous(breaks = seq(1:10))+
      theme_pablo
  }
  else if (x %in% df_struct$quality_vars & y %in% df_struct$quality_vars){
    plt <- ggplot(opros, aes(x = opros[[x]], fill = opros[[y]], color = I("black")))+
      geom_histogram(stat = "count")+
      facet_wrap(~ opros[[y]])+
      xlab(spaced_var_labels[x])+
      ylab("Количество")+
      ggtitle(paste("Взаимосвязь переменных\n", spaced_var_labels[x], "и",
                    spaced_var_labels[y]))+
      scale_fill_brewer(name = var_labels[y],
                        type = "qual", palette = sample(1:8, size=1))+
      theme_pablo
  }
  plt
}
draw_simple_plot <- function(x) {
  if (x %in% df_struct$quality_vars) {
    plt <- ggplot(opros, aes(x = opros[[x]], fill = opros[[x]], color = I("black")))+
      geom_histogram(stat = "count")+
      xlab(spaced_var_labels[x])+
      ylab("Количество")+
      ggtitle(paste("Распределение переменой\n", spaced_var_labels[x]))+
      scale_fill_brewer(name = var_labels[x],
                        type = "qual", palette = sample(1:8, size=1))+
      theme_pablo
  }
  else if (x %in% df_struct$numeric_vars) {
    random_color <- colors()[sample(1:length(colors()), size=1)]
    plt <- ggplot(opros, aes(x = opros[[x]], fill = I(random_color), color = I("black")))+
      geom_histogram(stat = "count")+
      xlab(spaced_var_labels[x])+
      ylab("Частота")+
      ggtitle(paste("Распределение переменной\n", spaced_var_labels[x]))+
      scale_fill_brewer(name = gsub(" ", "\n", var_labels[x]),
                        type = "qual", palette = sample(1:8, size=1), 
                        guide="colourbar")+
      scale_x_continuous(breaks = seq(1:10))+
      theme_pablo
  }
  plt
}
result$simple_plots <- lapply(opros, function(x) {
  x <- find_opros_col(x)
  plt <- draw_simple_plot(x)
  plt
})

draw_complex_plot <- function(x) {
  kit <- c()
  for (i in 1:length(unlist(df_struct$complex_vars[x]))) {
    y <- unlist(df_struct$complex_vars[x])[i]
    kit <- c(kit, 
             ifelse(opros[y] == var_positive_flags[x], full_var_labels[y], "none"))
  }
  kit <- data.frame(var = kit[kit != "none"])
  kit$var <- factor(kit$var)
  plt <- ggplot(kit, aes(x = var, fill = var, col = I("black")))+
    geom_histogram(stat = "count")+
    ggtitle(spaced_complex_var_labels[x])+
    xlab(spaced_complex_var_labels[x])+
    ylab("Частота")+
    scale_fill_brewer(name = "Частота",
                      type = "qual", palette = 3)+
    theme_pablo
  plt
}

result$complex_plots <- list()
result$complex_plots$languages <- draw_complex_plot("languages")
result$complex_plots$editors <- draw_complex_plot("editors")
result$complex_plots$futures <- draw_complex_plot("futures")

# gsub(" ", "_", gsub("\n ", "-", draw_plot("gender", "status")$labels$title))
# result$edged_vars
df <- data.frame(x = c(), y = c())
result$important_plots <- lapply(opros, function(x) {
  x <- find_opros_col(x)
  lapply(opros[unlist(result$edged_vars[x])], function(y) {
    y <- find_opros_col(y)
    plt <- draw_plot(x, y)
#      if (!(paste(x, y) %in% c(apply(df, 1, function(x) paste(x["x"], x["y"])),
#                             apply(df, 1, function(x) paste(x["y"], x["x"]))))) {
#      df <<- rbind(df, data.frame(x = x, y = y))
#      i <- gsub("\"", "", gsub(" ", "_", gsub("\n ", "-", plt$labels$title)))
#      ggsave(file = paste0(getwd(), "/plots/important_plots/", i, ".png"), 
#             plot = plt, device = "png")}
    plt})})

# Graph creating
library(igraph)
net <- list()
net$edges <- data.frame(from = result$important_edges$all$x,
                        to = result$important_edges$all$y,
                        weight = result$important_edges$all$p_log)
net$vertices <- data.frame(
  id = colnames(opros),
  type = ifelse(colnames(opros) %in% df_struct$numeric_vars, "numeric", "quality"))
graph <- graph.data.frame(net$edges, net$vertices, directed = F)
V(graph)$color <- ifelse(net$vertices$id %in% df_struct$numeric_vars, "orange", "skyblue")
E(graph)$width <- as.numeric(as.vector(net$edges$weight))
l <- layout.kamada.kawai(graph)
# result$egdes_graph <- plot(graph, layout= l, vertex.size = 15)

result$simple_fisher_test$gender
