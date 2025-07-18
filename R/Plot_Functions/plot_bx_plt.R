library(ggplot2)
library(ComplexHeatmap)

plot.bx.plt <- function(df){
  return(ggplot(data = melt(df), aes(x = variable, y = value)) +
    geom_boxplot(outlier.colour = "red", 
                 outlier.size = 1, 
                 fill = "cyan",  
                 color = NA,      
                 alpha = 2)  +
    geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") +
    stat_summary(fun = median, geom = "crossbar", 
                 width = 0.5, color = "blue", size = 1.5) + 
    theme_minimal() + 
    labs(x = NULL, y = 'Value') + 
    theme(axis.text.x = element_blank(),  
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank()) +
          # panel.grid.minor = element_blank()) + 
    theme(axis.text.y = element_text(size = 12)))
}

plot.dst.plt <- function(X, Y){
  
  nm <- names(X)
  if(class(X) != "list"){
    X <- list(X)
    nm <- NULL
  }
  
  plt.list <- lapply(1:length(X), function(i){
    bloc <- X[[i]] %>% 
      dplyr::select(where(is.numeric)) %>% 
      t()
    
    densityHeatmap(bloc,
                   column_split = Y,
                   ylab = nm[i])
  })
  return(plt.list)
} 
