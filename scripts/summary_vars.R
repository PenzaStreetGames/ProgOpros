languages_names <- names(languages)
editors_names <- names(editors)
future_names <- names(future)
opros$languages_number <- 0
for (i in languages_names) {
  opros$languages_number <- opros$languages_number + (opros[[i]] == "Использует")
}
opros$editors_number <- 0
for (i in editors_names) {
  opros$editors_number <- opros$editors_number + (opros[[i]] == "Использует")
}
opros$future_number <- 0
for (i in future_names) {
  opros$future_number <- opros$future_number + (opros[[i]] == "Ожидает")
}
