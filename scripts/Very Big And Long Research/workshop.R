library(ggplot2)
library(ggthemes)

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

warnings()
library(extrafont)
font_import()
loadfonts(device = "win")
windowsFonts()

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
