library(ggplot2)
draw_plot <- function(x, y) {
  if (x %in% df_struct$numeric_vars & y %in% df_struct$numeric_vars) {
    random_colors <- colors()[sample(1:length(colors()), size = 2, replace = F)]
    plt <- ggplot(opros, aes(x = opros[[x]], y = opros[[y]], 
                             size = I(5), color = I(random_colors[1])))+
      geom_smooth(method = "lm", se = F, color = I(random_colors[2]), size = I(3))+
      geom_point()+
      scale_x_continuous(breaks = seq(1:10))+
      geom_jitter()+
      xlab(var_labels[x])+
      ylab(var_labels[y])+
      ggtitle(paste("Взаимосвязь переменных\n", spaced_var_labels[x], "и",
                    spaced_var_labels[y]))+
      theme_pablo
  }
  else if (x %in% df_struct$numeric_vars & y %in% df_struct$quality_vars) {
    plt <- ggplot(opros, aes(x = opros[[x]], fill = opros[[y]], color = I("black")))+
      geom_histogram(alpha = 0.8, binwidth = 1)+
      facet_wrap(~ opros[[y]], nrow = 3)+
      xlab(var_labels[x])+
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
      xlab(var_labels[y])+
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
      xlab(var_labels[x])+
      ylab("Количество")+
      ggtitle(paste("Взаимосвязь переменных\n", spaced_var_labels[x], "и",
                    spaced_var_labels[y]))+
      scale_fill_brewer(name = var_labels[y],
                        type = "qual", palette = sample(1:8, size=1))+
      theme_pablo
  }
  plt
}
gsub(" ", "_", gsub("\n ", "-", draw_plot("gender", "status")$labels$title))
result$edged_vars
df <- data.frame(x = c(), y = c())
result$important_plots <- lapply(opros, function(x) {
  x <- find_opros_col(x)
  lapply(opros[unlist(result$edged_vars[x])], function(y) {
    y <- find_opros_col(y)
    plt <- draw_plot(x, y)
    if (!(paste(x, y) %in% c(apply(df, 1, function(x) paste(x["x"], x["y"])),
                             apply(df, 1, function(x) paste(x["y"], x["x"]))))) {
      df <<- rbind(df, data.frame(x = x, y = y))
      i <- gsub("\"", "", gsub(" ", "_", gsub("\n ", "-", plt$labels$title)))
      ggsave(file = paste0(getwd(), "/plots/important_plots/", i, ".png"), 
             plot = plt, device = "png")}
    plt})})
result$important_plots$gender$status
