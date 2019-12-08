library(ggplot2)
library(extrafont)
loadfonts(device = "win")

theme_pablo <- theme(panel.background = element_rect(fill = "grey70"), 
                     plot.background = element_rect(fill = "grey60"),
                     legend.background = element_rect(fill = "grey80"),
                     text = element_text(family = "Comic Sans MS"),
                     axis.text.x = element_text(angle = 30, hjust = 1))

ggplot(opros, aes(x = cpp, fill = cpp, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ editor_theme)+
  scale_fill_brewer(type = "qual", palette = 7)+
  xlab("Знание С++")+
  ylab("Частота")+
  theme(panel.background = element_rect(fill = "grey70"), 
        plot.background = element_rect(fill = "grey60"),
        legend.background = element_rect(fill = "grey80"),
        text = element_text(family = "Comic Sans MS"))

ggplot(opros, aes(x = microboard, fill = microboard, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_grid(~ desctop_os)+
  scale_fill_brewer(type = "qual", palette = 7)+
  xlab("Выбор настольной ОС")+
  ylab("Частота")+
  theme_pablo

ggplot(opros, aes(x = microboard, fill = microboard, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ cpp)+
  scale_fill_brewer(type = "qual", palette = 7)+
  xlab("Использование С++")+
  ylab("Частота")+
  theme_pablo

ggplot(opros, aes(x = languages_number, fill = microboard, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ microboard, nrow = 3)+
  xlab("Количество используемых языков")+
  scale_fill_brewer(type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  ylab("Частота")+
  theme_pablo

ggplot(opros, aes(x = editor_theme, fill = editor_theme, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ cpp)+
  scale_fill_brewer(type = "qual", palette = 7)+
  xlab("Использование С++")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = editor_theme, fill = editor_theme, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ notepad)+
  scale_fill_brewer(type = "qual", palette = 7)+
  xlab("Использование Блокнота")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = editor_theme, fill = editor_theme, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_grid(notepad ~ cpp)+
  scale_fill_brewer(type = "qual", palette = 7)+
  xlab("Использование С++")+
  ylab("Использование Блокнота")+
  theme_pablo

ggplot(opros, aes(x = processor, fill = processor, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ status)+
  scale_fill_brewer(type = "qual", palette = 8)+
  xlab("Социальный статус")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = fasm, fill = fasm, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ status)+
  scale_fill_brewer(type = "qual", palette = 8)+
  xlab("Социальный статус")+
  ylab("Количество")+
  theme_pablo

result$double_tables$status$fasm

ggplot(opros, aes(x = future_number, fill = status, color = I("black")))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~ status)+
  scale_fill_brewer(type = "qual", palette = 8, name = "Социальный статус")+
  scale_x_continuous(breaks = seq(1:6))+
  xlab("Количество перспективных отраслей")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = future_number, fill = status, color = I("black")))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~ status)+
  scale_fill_brewer(type = "qual", palette = 8, name = "Социальный статус")+
  scale_x_continuous(breaks = seq(1:6))+
  xlab("Количество перспективных отраслей")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = languages_number, y = dont_know, size = I(5), color = I("tomato4")))+
  geom_smooth(method = "lm", se = F, color = I("indianred2"), size = I(3))+
  geom_point()+
  scale_x_continuous(breaks = seq(1:10))+
  geom_jitter()+
  xlab("Количество используемых языков")+
  ylab("Коэффициент незнания")+
  theme_pablo

result$cor_test_kendal$languages_number$dont_know

ggplot(opros, aes(x = dont_know, fill = python, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ python, nrow = 3)+
  xlab("Коэффициент незнания")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование Python",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo

ggplot(opros, aes(x = sugar_using, y = dont_know, size = I(5), color = I("tomato4")))+
  geom_smooth(method = "lm", se = F, color = I("indianred2"), size = I(3))+
  geom_point()+
  scale_x_continuous(breaks = seq(1:10))+
  geom_jitter()+
  xlab("Использование синтаксического сахара")+
  ylab("Коэффициент незнания")+
  theme_pablo

ggplot(opros, aes(x = dont_know, fill = scratch, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ scratch, nrow = 3)+
  xlab("Коэффициент незнания")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование Scratch",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo

ggplot(opros, aes(x = sugar_using, fill = pycharm, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ pycharm, nrow = 3)+
  xlab("Использование синтаксического сахара")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование PyCharm",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo

ggplot(opros, aes(x = wing, fill = slow_python, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ slow_python)+
  scale_fill_brewer(name = "Отношение к\nмедленному питону",
                    type = "qual", palette = 8)+
  xlab("Использование Wing")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = languages_number, fill = typing, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ typing, nrow = 3)+
  xlab("Количество используемых языков")+
  ylab("Частота")+
  scale_fill_brewer(name = "Мнение о типизации",
                    type = "qual", palette = 3)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo

ggplot(opros, aes(x = languages_number, fill = java_kotlin, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ java_kotlin, nrow = 3)+
  xlab("Количество используемых языков")+
  ylab("Частота")+
  scale_fill_brewer(name = "Java или Kotlin?",
                    type = "qual", palette = 3)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo

ggplot(opros, aes(x = editors_number, fill = pascal, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ pascal, nrow = 3)+
  xlab("Количество используемых редакторов")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование Pascal",
                    type = "qual", palette = 3)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo

ggplot(opros, aes(x = languages_number, fill = back_front_end, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ back_front_end, nrow = 3)+
  xlab("Количество используемых языков")+
  ylab("Частота")+
  scale_fill_brewer(name = "Отрасль web'а",
                    type = "qual", palette = 3)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo

ggplot(opros, aes(x = editors_number, fill = back_front_end, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ back_front_end, nrow = 3)+
  xlab("Количество используемых редакторов")+
  ylab("Частота")+
  scale_fill_brewer(name = "Отрасль web'а",
                    type = "qual", palette = 3)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo

ggplot(opros, aes(x = editor_theme, fill = editor_theme, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ processor)+
  scale_fill_brewer(name = "Тема редактора",
                    type = "qual", palette = 1)+
  xlab("Выбор процессора")+
  ylab("Количество")+
  theme_pablo
###
ggplot(opros, aes(x = gender, fill = gender, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ pycharm)+
  scale_fill_brewer(name = "Пол",
                    type = "qual", palette = 1)+
  xlab("Использование PyCharm")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = languages_number, fill = cycle, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ cycle, nrow = 3)+
  xlab("Количество используемых языков")+
  ylab("Частота")+
  scale_fill_brewer(name = "Выбор цикла",
                    type = "qual", palette = 6)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo

ggplot(opros, aes(x = cycle, fill = cycle, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ typing)+
  scale_fill_brewer(name = "Сколько будет 1 + \"а\"?",
                    type = "qual", palette = 1)+
  xlab("Выбор цикла")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = back_front_end, fill = back_front_end, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ gender)+
  scale_fill_brewer(name = "Выбор отрасли web'а",
                    type = "qual", palette = 1)+
  xlab("Пол")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = gender, fill = gender, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ csharp)+
  scale_fill_brewer(name = "Пол",
                    type = "qual", palette = 1)+
  xlab("Использование C#")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = gender, fill = gender, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ notepad)+
  scale_fill_brewer(name = "Пол",
                    type = "qual", palette = 1)+
  xlab("Использование Блокнота")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = gender, fill = gender, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ jupiter)+
  scale_fill_brewer(name = "Пол",
                    type = "qual", palette = 1)+
  xlab("Использование Jupiter Notebook")+
  ylab("Количество")+
  theme_pablo

result$double_tables$jupiter$gender

ggplot(opros, aes(x = gender, fill = gender, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ indexing)+
  scale_fill_brewer(name = "Пол",
                    type = "qual", palette = 1)+
  xlab("Выбор индексации")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = sugar, fill = sugar, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ gender)+
  scale_fill_brewer(name = "Количество сахара\nв питоне",
                    type = "qual", palette = 1)+
  xlab("Пол")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = list_expressions, fill = list_expressions, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ gender)+
  scale_fill_brewer(name = "Использование\nсписочных\nвыражений",
                    type = "qual", palette = 1)+
  xlab("Пол")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = gender, fill = gender, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ php)+
  scale_fill_brewer(name = "Пол",
                    type = "qual", palette = 1)+
  xlab("Использование PHP")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = gender, fill = gender, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ go)+
  scale_fill_brewer(name = "Пол",
                    type = "qual", palette = 1)+
  xlab("Использование Go")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = gender, fill = gender, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ status)+
  scale_fill_brewer(name = "Статус",
                    type = "qual", palette = 7)+
  xlab("Пол")+
  ylab("Количество")+
  theme_pablo
###
ggplot(opros, aes(x = notepad, fill = notepad, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ status)+
  scale_fill_brewer(name = "Использование\nБлокнота",
                    type = "qual", palette = 7)+
  xlab("Социальный статус")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = java_kotlin, fill = java_kotlin, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ status)+
  scale_fill_brewer(name = "Java или Kotlin",
                    type = "qual", palette = 7)+
  xlab("Социальный статус")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = jupiter, fill = jupiter, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ status)+
  scale_fill_brewer(name = "Использование\nJupiter\nNotebook",
                    type = "qual", palette = 7)+
  xlab("Социальный статус")+
  ylab("Количество")+
  theme_pablo

ggplot(opros, aes(x = languages_number, fill = jupiter, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ jupiter, nrow = 3)+
  xlab("Количество используемых языков")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nJupiter\nNotebook",
                    type = "qual", palette = 3)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo

ggplot(opros, aes(x = languages_number, fill = atom, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ atom, nrow = 3)+
  xlab("Количество используемых языков")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование Atom'а",
                    type = "qual", palette = 3)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo

ggplot(opros, aes(x = humour, fill = desctop_os, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 3)+
  facet_grid(~ desctop_os)+
  xlab("Коэффициент юмора")+
  ylab("Частота")+
  scale_fill_brewer(name = "Выбор\nнастольной ОС",
                    type = "qual", palette = 3)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo

ggplot(opros, aes(x = humour, fill = mobile_os, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 3)+
  facet_grid(~ mobile_os)+
  xlab("Коэффициент юмора")+
  ylab("Частота")+
  scale_fill_brewer(name = "Выбор\nмобильной ОС",
                    type = "qual", palette = 3)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo

ggplot(opros, aes(x = humour, fill = sugar, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 3)+
  facet_grid(~ sugar)+
  xlab("Коэффициент юмора")+
  ylab("Частота")+
  scale_fill_brewer(name = "Количество сахара\nв питоне",
                    type = "qual", palette = 5)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo

ggplot(opros, aes(x = humour, fill = sugar, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 3)+
  facet_wrap(~ sugar)+
  xlab("Коэффициент юмора")+
  ylab("Частота")+
  scale_fill_brewer(name = "Количество сахара\nв питоне",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo

ggplot(opros, aes(x = humour, fill = indexing, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 3)+
  facet_wrap(~ indexing)+
  xlab("Коэффициент юмора")+
  ylab("Частота")+
  scale_fill_brewer(name = "Выбор индексации",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo

ggplot(opros, aes(x = humour, fill = cpp, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 3)+
  facet_wrap(~ cpp)+
  xlab("Коэффициент юмора")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование С++",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo

ggplot(opros, aes(x = humour, fill = csharp, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 3)+
  facet_wrap(~ csharp)+
  xlab("Коэффициент юмора")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование С#",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo
#?

ggplot(opros, aes(x = humour, fill = go, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 3)+
  facet_wrap(~ go)+
  xlab("Коэффициент юмора")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование Go",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo
#?

ggplot(opros, aes(x = humour, fill = pycharm, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 3)+
  facet_wrap(~ pycharm)+
  xlab("Коэффициент юмора")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование PyCharm",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo
#?

ggplot(opros, aes(x = humour, fill = idle, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 3)+
  facet_wrap(~ idle)+
  xlab("Коэффициент юмора")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование IDLE",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo
#?

ggplot(opros, aes(x = humour, fill = notepad, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 3)+
  facet_wrap(~ notepad)+
  xlab("Коэффициент юмора")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nБлокнота",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo
#!

ggplot(opros, aes(x = humour, fill = quantum, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 3)+
  facet_wrap(~ quantum)+
  xlab("Коэффициент юмора")+
  ylab("Частота")+
  scale_fill_brewer(name = "Ожиадние развития\nквантовой логики",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo
#?

ggplot(opros, aes(x = web_using, y = dont_know, size = I(5), color = I("tomato4")))+
  geom_smooth(method = "lm", se = F, color = I("indianred2"), size = I(3))+
  geom_point()+
  scale_x_continuous(breaks = seq(1:10))+
  geom_jitter()+
  xlab("Использование web'а")+
  ylab("Коэффициент незнания")+
  theme_pablo
#!

ggplot(opros, aes(x = dont_know, fill = java_kotlin, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 2)+
  facet_wrap(~ java_kotlin, nrow = 3)+
  xlab("Коэффициент незнания")+
  ylab("Частота")+
  scale_fill_brewer(name = "Java или Kotlin",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo
#+-

###
ggplot(opros, aes(x = other_opinion, fill = microboard, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ microboard)+
  xlab("Коэффициент другого мнения")+
  ylab("Частота")+
  scale_fill_brewer(name = "Выбор микроплаты",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo
#?

ggplot(opros, aes(x = middle_answers, fill = atom, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ atom)+
  xlab("Коэффициент другого мнения")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование Atom'а",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo
#?

ggplot(opros, aes(x = python_discontent, fill = list_expressions, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ list_expressions)+
  xlab("Коэффициент недовольства питоном")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nсписочных\nвыражений",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo
#?

ggplot(opros, aes(x = python_discontent, fill = python, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ python)+
  xlab("Коэффициент недовольства питоном")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nPython",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo
#?

ggplot(opros, aes(x = python_discontent, fill = big_data, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ big_data)+
  xlab("Коэффициент недовольства питоном")+
  ylab("Частота")+
  scale_fill_brewer(name = "Ожиадние развития\nBig Data",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(0, 10, round(10/3)))+
  theme_pablo
result$double_tables$python_discontent$big_data
#+-

workers <- opros[opros$status == "Работяга", c("machine_learning", "big_data",
                                               "metaprog", "quantum", "cryptography",
                                               "math")]
futures <- c(ifelse(workers$machine_learning == "Ожидает", "machine_learning", "none"),
             ifelse(workers$big_data == "Ожидает", "big_data", "none"),
             ifelse(workers$metaprog == "Ожидает", "metaprog", "none"),
             ifelse(workers$quantum == "Ожидает", "quantum", "none"),
             ifelse(workers$cryptography == "Ожидает", "cryptography", "none"),
             ifelse(workers$math == "Ожидает", "math", "none"))
futures <- as.data.frame(futures[futures != "none"])
colnames(futures) <- c("future")
futures$future <- factor(futures$future, labels = c("Большие данные", "Криптография",
                                                    "Машинное обучение", 
                                                    "Метапрограммирование",
                                                    "Квантовая логика"))
futures
ggplot(futures, aes(x = future, fill = future, col = I("black")))+
  geom_histogram(stat = "count")+
  xlab("Отрасли будущего по мнению работяг")+
  ylab("Частота")+
  scale_fill_brewer(name = "Отрасли",
                    type = "qual", palette = 3)+
  theme_pablo
#!

ggplot(opros, aes(x = web_using, y = sugar_using, size = I(5), color = I("tomato4")))+
  geom_smooth(method = "lm", se = F, color = I("indianred2"), size = I(3))+
  geom_point()+
  scale_x_continuous(breaks = seq(1:10))+
  geom_jitter()+
  xlab("Использование web'а")+
  ylab("Использование синтаксического сахара")+
  theme_pablo
#!

ggplot(opros, aes(x = sugar_using, fill = zero_division, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ zero_division, nrow = 3)+
  xlab("Использование синтаксического сахара")+
  ylab("Частота")+
  scale_fill_brewer(name = "Взгляд\nна деление\nна ноль",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#?

ggplot(opros, aes(x = sugar_using, fill = flask_django, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ flask_django, nrow = 2)+
  xlab("Использование синтаксического сахара")+
  ylab("Частота")+
  scale_fill_brewer(name = "Flask или Django?",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

ggplot(opros, aes(x = sugar_using, fill = python, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ python, nrow = 2)+
  xlab("Использование синтаксического сахара")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nPython",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#+-

ggplot(opros, aes(x = sugar_using, fill = cpp, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ cpp, nrow = 2)+
  xlab("Использование синтаксического сахара")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование C++",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

###
ggplot(opros, aes(x = web_using, fill = java_kotlin, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 6)+
  facet_wrap(~ java_kotlin, nrow = 3)+
  xlab("Использование web'а")+
  ylab("Частота")+
  scale_fill_brewer(name = "Java или Kotlin?",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

ggplot(opros, aes(x = web_using, fill = list_mutable, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 6)+
  facet_wrap(~ list_mutable, nrow = 3)+
  xlab("Использование web'а")+
  ylab("Частота")+
  scale_fill_brewer(name = "Отношение к\nизменяемости\nсписков в питоне",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#+-

ggplot(opros, aes(x = web_using, fill = list_expressions, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 6)+
  facet_wrap(~ list_expressions, nrow = 3)+
  xlab("Использование web'а")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nсписочных\nвыражений",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#?

ggplot(opros, aes(x = web_using, fill = ternar_module, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 6)+
  facet_wrap(~ ternar_module, nrow = 3)+
  xlab("Использование web'а")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nтернарного\nмодуля",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#?

ggplot(opros, aes(x = web_using, fill = patterns, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 6)+
  facet_wrap(~ patterns, nrow = 3)+
  xlab("Использование web'а")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nшаболнов\nпроектирования",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#?

ggplot(opros, aes(x = web_using, fill = python, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 6)+
  facet_wrap(~ python, nrow = 3)+
  xlab("Использование web'а")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nPython",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

ggplot(opros, aes(x = web_using, fill = javascript, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 6)+
  facet_wrap(~ javascript, nrow = 3)+
  xlab("Использование web'а")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nJavaScript",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

ggplot(opros, aes(x = web_using, fill = pascal, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 6)+
  facet_wrap(~ pascal, nrow = 3)+
  xlab("Использование web'а")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nPascal",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

ggplot(opros, aes(x = web_using, fill = lua, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 6)+
  facet_wrap(~ lua, nrow = 3)+
  xlab("Использование web'а")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nLua",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

ggplot(opros, aes(x = web_using, fill = fasm, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 6)+
  facet_wrap(~ fasm, nrow = 3)+
  xlab("Использование web'а")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nFASM",
                    type = "qual", palette = 4)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

View(opros[opros$fasm == "Использует",])

ggplot(opros, aes(x = web_using, fill = bf, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 6)+
  facet_wrap(~ bf, nrow = 3)+
  xlab("Использование web'а")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nBrainfuck",
                    type = "qual", palette = 4)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

ggplot(opros, aes(x = web_using, fill = pycharm, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 6)+
  facet_wrap(~ pycharm, nrow = 3)+
  xlab("Использование web'а")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nPyCharm",
                    type = "qual", palette = 4)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#?

ggplot(opros, aes(x = web_using, fill = jupiter, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 6)+
  facet_wrap(~ jupiter, nrow = 3)+
  xlab("Использование web'а")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nJupiter\nNotebook",
                    type = "qual", palette = 4)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

ggplot(opros, aes(x = web_using, fill = atom, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 6)+
  facet_wrap(~ atom, nrow = 3)+
  xlab("Использование web'а")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование\nAtom",
                    type = "qual", palette = 4)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

ggplot(opros, aes(x = web_using, fill = machine_learning, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 6)+
  facet_wrap(~ machine_learning, nrow = 3)+
  xlab("Использование web'а")+
  ylab("Частота")+
  scale_fill_brewer(name = "Ожидание\nравития\nмашинного\nобучения",
                    type = "qual", palette = 4)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#+-

ggplot(opros, aes(x = apple, fill = gender, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 3)+
  facet_wrap(~ gender, ncol = 3)+
  xlab("Коэффициент лояльности к Apple")+
  ylab("Частота")+
  scale_fill_brewer(name = "Пол",
                    type = "qual", palette = 4)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

ggplot(opros, aes(x = apple, fill = math, color = I("black")))+
  geom_histogram(alpha = 0.8, bins = 3)+
  facet_wrap(~ math, ncol = 3)+
  xlab("Коэффициент лояльности к Apple")+
  ylab("Частота")+
  scale_fill_brewer(name = "Ожидание\nразвития\nприкладной\nматематики",
                    type = "qual", palette = 4)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

ggplot(opros, aes(x = middle_answers, fill = go, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ go, nrow = 3)+
  xlab("Коэффициент лояльности ответов")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование Go",
                    type = "qual", palette = 4)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#+-

ggplot(opros, aes(x = future_number, fill = python, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ python, nrow = 3)+
  xlab("Количество перспективных отраслей")+
  ylab("Количество")+
  scale_fill_brewer(name = "Использование\nPython",
                    type = "qual", palette = 8)+
  scale_x_continuous(breaks = seq(1:6))+
  theme_pablo
#!

ggplot(opros, aes(x = future_number, fill = ruby, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ ruby, nrow = 3)+
  xlab("Количество перспективных отраслей")+
  ylab("Количество")+
  scale_fill_brewer(name = "Использование\nRuby",
                    type = "qual", palette = 8)+
  scale_x_continuous(breaks = seq(1:6))+
  theme_pablo
#!

ggplot(opros, aes(x = mobile_os, fill = mobile_os, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~ gender)+
  scale_fill_brewer(name = "Пол",
                    type = "qual", palette = 7)+
  xlab("Выбор мобтльной ОС")+
  ylab("Количество")+
  theme_pablo
#!!!

ggplot(opros, aes(x = other_opinion, fill = lua, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ lua)+
  xlab("Коэффициент другого мнения")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование Lua",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(1, 10))+
  theme_pablo
#+-

ggplot(opros, aes(x = other_opinion, fill = atom, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ atom)+
  xlab("Коэффициент другого мнения")+
  ylab("Частота")+
  scale_fill_brewer(name = "Использование Atom",
                    type = "qual", palette = 1)+
  scale_x_continuous(breaks = seq(1, 10))+
  theme_pablo
#+-

ggplot(opros, aes(x = editors_number, fill = cryptography, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ cryptography, nrow = 3)+
  xlab("Количество используемых редакторов")+
  ylab("Частота")+
  scale_fill_brewer(name = "Ожидание\nразвития\nкриптографии",
                    type = "qual", palette = 3)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#?

###
ggplot(opros, aes(x = sugar_using, fill = web, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ web, nrow = 3)+
  xlab("Использование синтаксического сахара")+
  ylab("Частота")+
  scale_fill_brewer(name = "Отношение к web'у",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#?

ggplot(opros, aes(x = dont_know, fill = processor, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ processor, nrow = 3)+
  xlab("Коэффициент незнания")+
  ylab("Частота")+
  scale_fill_brewer(name = "Выбор процессора",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

ggplot(opros, aes(x = dont_know, fill = java_kotlin, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ java_kotlin, nrow = 3)+
  xlab("Коэффициент незнания")+
  ylab("Частота")+
  scale_fill_brewer(name = "Java или Kotlin?",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

ggplot(opros, aes(x = dont_know, fill = web, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ web, nrow = 3)+
  xlab("Коэффициент незнания")+
  ylab("Частота")+
  scale_fill_brewer(name = "Отношение к web'у",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
#!

ggplot(opros, aes(x = dont_know, fill = gender, color = I("black")))+
  geom_histogram(alpha = 0.8, binwidth = 1)+
  facet_wrap(~ gender, nrow = 3)+
  xlab("Коэффициент незнания")+
  ylab("Частота")+
  scale_fill_brewer(name = "",
                    type = "qual", palette = 7)+
  scale_x_continuous(breaks = seq(1:10))+
  theme_pablo
