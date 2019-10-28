languages <- c(python = "Python",
               cpp = "C\\+\\+",
               javascript = "Java Script",
               pascal = "Pascal",
               csharp = "C#",
               java = "Java[,$]",
               c = "C[,$]",
               php = "PHP",
               kotlin = "Kotlin",
               lua = "Lua",
               scratch = "Scratch",
               basic = "Basic",
               go = "Go",
               ruby = "Ruby",
               fasm = "FASM",
               bf = "Brainfuck",
               haskel = "Haskel")
for (i in 1:length(languages)) {
  opros[[names(languages)[i]]] <- ifelse(grepl(languages[i], opros$languages), "Использует", "Не использует")
}
editors <- c(pycharm = "PyCharm",
             vscode = "Visual Studio Code",
             idle = "IDLE",
             notepad = "Блокнот",
             notepadpp = "Notepad\\+\\+",
             wing = "Wing",
             sublime = "Sublime text",
             jupiter = "Jupiter Notebook",
             atom = "Atom",
             console = "Консоль")
for (i in 1:length(editors)) {
  opros[[names(editors)[i]]] <- ifelse(grepl(editors[i], opros$editors), "Использует", "Не использует")
}
future <- c(machine_learning = "Машинное обучение",
            big_data = "Большие данные",
            metaprog = "Метапрограммирование",
            quantum = "Квантовая логика",
            cryptography = "Криптография",
            math = "Прикладная математика")
for (i in 1:length(future)) {
  opros[[names(future)[i]]] <- ifelse(grepl(future[i], opros$future), "Ожидает", "Не ожидает")
}
sapply(opros[27:53], table)
sapply(opros[54:59], table)
colnames(opros)
cols_to_delete <- c("languages", "editors", "future") 
for (i in cols_to_delete) {
  opros[[i]] <- NULL
}