opros$humour <- (opros$gender == "Кафельный") + (opros$status == "Пенсионер") + 
  (opros$editor_theme == "Не фильтрованная")
opros$other_opinion <- (opros$zero_division == "Так нельзя делать") + 
  (opros$zero_division == "Бесконечность") * 2 + 
  (opros$indexing == "С единицы") + (opros$indexing == "С произвольного числа") * 2 + 
  (opros$typing == "1a") + (opros$typing == '98 (1 + ord("a"))') * 2
opros$python_discontent <- ((opros$slow_python == "Терпимо") + 
                              (opros$slow_python == "Побыстрее бы") * 2 +
                              (opros$list_mutable == "Нейтрально") + 
                              (opros$list_mutable == "Негативно") * 2 +
                              (opros$sugar == "Достаточно") + 
                              (opros$sugar == "Можно было бы послаще") * 2) * 
  (opros$python == "Использует")
opros$sugar_using <- (opros$list_expressions == "Редко") + 
  (opros$list_expressions == "Часто") * 2 +
  (opros$ternar_module == "Редко") +
  (opros$ternar_module == "Часто") * 2 +
  (opros$patterns == "В питоне не применимы") +
  (opros$patterns == "Редко") * 2 +
  (opros$patterns == "Часто") * 3
