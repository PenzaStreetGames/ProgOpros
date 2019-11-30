library(dplyr)
library(ggplot2)
library(ggthemes)

rownames(mini_set) <- NULL
mini_set <- opros[c("gender", "languages_number", "back_front_end")]
write.csv(mini_set, file = "scripts/report/article_data.csv", fileEncoding = "UTF-8")

poll <- read.csv("article_data.csv")
poll["X"] <- NULL
head(poll)
ggplot(poll, aes(back_front_end, fill = back_front_end, color = I("black")))+
  geom_histogram(stat = "count")+
  facet_wrap(~gender)+
  scale_fill_discrete(name = "Отрасль web'а")+
  xlab("")+
  ylab("")+
  theme(panel.background = element_rect(fill = "grey80"), 
          plot.background = element_rect(fill = "grey70"),
        legend.background = element_rect(fill = "grey90"))
colors()

ggplot(poll, aes(languages_number, fill = back_front_end, color = I("black")))+
  geom_density()+
  facet_wrap(~back_front_end, nrow = 3)+
  scale_x_continuous(name = "Количество языков программирования",
                     breaks = c(1:10))+
  scale_fill_discrete(name = "Отрасль web'а")+
  ylab("")+
  theme(panel.background = element_rect(fill = "grey80"), 
        plot.background = element_rect(fill = "grey70"),
        legend.background = element_rect(fill = "grey90"))
?pvalue

ggplot()
