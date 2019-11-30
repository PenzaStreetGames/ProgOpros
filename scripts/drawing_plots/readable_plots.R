library(ggplot2)

ggplot(opros, aes(sugar_using, fill = I("orange"))) +
  geom_density()+
  facet_grid(wing ~ slow_python)

ggplot(opros, aes(editors_number, fill = pascal)) +
  geom_density()+
  facet_grid(. ~ pascal)

ggplot(opros, aes(languages_number, dont_know, color = microboard)) +
  geom_point()+
  geom_jitter()

ggplot(opros, aes(mobile_os, fill = mobile_os, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_grid(gender ~ status)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(opros, aes(gender, fill = gender, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_grid(mobile_os ~ desctop_os)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(opros, aes(status, fill = status, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_grid(mobile_os ~ desctop_os)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
table(opros$status)

ggplot(opros, aes(python_discontent, color = processor)) +
  geom_density() +
  facet_grid(processor ~ .)

idle_lab <- factor(opros$idle, labels = c("idle+", "idle-"))
ggplot(opros, aes(console, fill = console, color = I("black"))) +
  geom_histogram(stat = "count") +
  facet_grid(factor(opros$notepad, labels= c("notepad+", "notepad-")) ~ 
               factor(opros$idle, labels = c("idle+", "idle-")))
