synthetic_max_values <- c("humour" = 3,
                "other_opinion" = 6,
                "python_discontent" = 6,
                "sugar_using" = 7,
                "apple" = 2,
                "middle_answers" = 11,
                "dont_know" = 4,
                "web_using" = 5)
add_sythetic_vars <- function(opros) {
  opros$humour <- (opros$gender == "Кафельный") + 
    (opros$status == "Пенсионер") +
    (opros$editor_theme == "Не фильтрованная")
  opros$other_opinion <- (opros$zero_division == "Так нельзя делать") +
    (opros$zero_division == "Бесконечность") * 2 +
    (opros$indexing == "С единицы") + 
    (opros$indexing == "С произвольного числа") * 2 +
    (opros$typing == "1a") + 
    (opros$typing == '98 (1 + ord("a"))') * 2
  opros$python_discontent <- ((opros$slow_python == "Терпимо") +
                                (opros$slow_python == "Побыстрее бы") * 2 +
                                (opros$list_mutable == "Нейтрально") +
                                (opros$list_mutable == "Негативно") * 2 +
                                (opros$sugar == "Достаточно") +
                                (opros$sugar == "Можно было бы послаще") * 2
  ) * (opros$python == "Использует")
  opros$sugar_using <- (opros$list_expressions == "Редко") +
    (opros$list_expressions == "Часто") * 2 +
    (opros$ternar_module == "Редко") +
    (opros$ternar_module == "Часто") * 2 +
    (opros$patterns == "В питоне не применимы") +
    (opros$patterns == "Редко") * 2 +
    (opros$patterns == "Часто") * 3
  opros$apple <- (opros$mobile_os == "iOS") +
    (opros$desctop_os == "macOS")
  opros$middle_answers <- (opros$processor == "Всё равно") +
    (opros$desctop_os == "Лишь бы был комп") +
    (opros$mobile_os == "Лишь бы был телефон") +
    (opros$cycle_recursion == "Когда как") +
    (opros$cycle == "Одинаково") +
    (opros$java_kotlin == "Не играет роли") +
    (opros$slow_python == "Нормально") +
    (opros$list_mutable == "Нейтрально") +
    (opros$sugar == "Достаточно") +
    (opros$mobile_desctop == "Всё равно") +
    (opros$flask_django == "Оба потянут")
  opros$dont_know <- (opros$microboard == "Не, не слышал") +
    (opros$list_expressions == "Не знал о них") +
    (opros$ternar_module == "Не знал") +
    (opros$patterns == "Не знал о них") +
    (opros$flask_django == "Не знаю")
  opros$web_using <- (opros$web == "Как прилагающееся") +
    (opros$web == "Конечно") * 2 +
    (opros$flask_django == "Flask") +
    (opros$flask_django == "Django") * 2 +
    (opros$flask_django == "Оба потянут") * 3
  for (name in names(synthetic_max_values)) {
    opros[[name]] <- round(opros[[name]] / synthetic_max_values[name] * 10, digits = 1)
  }
  return(opros)
}
