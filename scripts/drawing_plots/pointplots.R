library(ggplot2)
having_plots <- c()
for (name_x in numeric_vars) {
  for (name_y in numeric_vars) {
    name <- paste(name_x, "~", name_y)
    if (name_x != name_y & !(name %in% having_plots)) {
      p <- ggplot(opros, aes(x = opros[[name_x]], y = opros[[name_y]], 
                                color = "red", size = 5))+
        geom_point()+
        geom_jitter(width = jitter_range, height = jitter_range)+
        xlab(name_x)+
        ylab(name_y)+
        ggtitle(name)
      ggsave(file = paste0(getwd(), "/plots/numeric_pointplots/", name, ".png"), plot = p, device = "png")
      having_plots <- c(having_plots, name, paste(name_y, "~", name_x))
    }
  }
}
having_plots <- c()
for (name_x in numeric_vars) {
  for (name_y in numeric_vars) {
    name <- paste(name_x, "~", name_y)
    if (name_x != name_y & !(name %in% having_plots)) {
      p <- ggplot(opros, aes(x = opros[[name_x]], y = opros[[name_y]], 
                             color = opros_c5$cluster, size = 5))+
        geom_point()+
        geom_jitter(width = jitter_range, height = jitter_range)+
        xlab(name_x)+
        ylab(name_y)+
        ggtitle(name)
      ggsave(file = paste0(getwd(), "/plots/clustered_pointplots/", name, ".png"), plot = p, device = "png")
      having_plots <- c(having_plots, name, paste(name_y, "~", name_x))
    }
  }
}
