library(dplyr)
library(tibble)
library(ggplot2)

source("Plot_Functions/plot_CV.R")
source("Plot_Functions/plot_IQR.R")
source("Plot_Functions/plot_freqCut.R")
source("Plot_Functions/plot_uniqueCut.R")

plot.metric <- function(df, metric, threshold){
  
  V <- NULL
  
  if (metric == "cv"){
    V <- plot.cv(df)
  }
  
  if (metric == "iqr"){
    V <- plot.iqr(df)
  }
  
  if (metric == "freqCut"){
    V <- plot.freqCut(df)
  }
  
  if (metric == "uniqueCut"){
    V <- plot.uniqueCut(df)
  }
  
  quartiles <- quantile(V$value, probs = c(0.25, 0.5, 0.75))
  
  p <- ggplot(V, aes(x = value)) +
    geom_histogram(aes(y=..density..), position = "dodge", fill = "lightblue")+
    geom_vline(xintercept = quartiles[1], linetype = "dashed", color = "blue") +  
    geom_vline(xintercept = quartiles[2], linetype = "solid", color = "tomato") +  
    geom_vline(xintercept = quartiles[3], linetype = "dashed", color = "blue") +
    theme_minimal()  
  
  p_build <- ggplot_build(p)
  max_y <- max(p_build$data[[1]]$density, na.rm = T)
  
  if (is.null(threshold)){
    return(p + 
             annotate("text", x = quartiles[1], y = max_y, label = "0.25", vjust = -1, color = "blue") +  
             annotate("text", x = quartiles[2], y = max_y * 0.8, label = "0.5", vjust = -1, color = "tomato") +  
             annotate("text", x = quartiles[3], y = max_y, label = "0.75", vjust = -1, color = "blue"))
  } else {
    qt <- quantile(V$value, na.rm = T, probs = threshold)
    V <- V %>% 
      mutate(keep = if (metric != "freqCut") {ifelse(V$value >= qt, "Yes", "N0")}else{ifelse(V$value <= qt, "Yes", "N0")})
    
    return(ggplot(V, aes(x = value, fill = keep)) +
             scale_fill_manual(values = c("Yes" = "#00BFC4", "N0" = "#F8766D")) +
             geom_histogram(aes(y=..density..), binwidth = 0.5, position = "identity") +
             geom_vline(xintercept = quartiles[1], linetype = "dashed", color = "blue") +  
             geom_vline(xintercept = quartiles[2], linetype = "solid", color = "tomato") +  
             geom_vline(xintercept = quartiles[3], linetype = "dashed", color = "blue") +
             annotate("text", x = quartiles[1], y = max_y, label = "0.25", vjust = -1, color = "blue") +  
             annotate("text", x = quartiles[2], y = max_y * 0.8, label = "0.5", vjust = -1, color = "tomato") +  
             annotate("text", x = quartiles[3], y = max_y, label = "0.75", vjust = -1, color = "blue") +
             theme_minimal())}
  
}