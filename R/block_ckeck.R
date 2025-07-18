library(gridExtra)
library(reshape2)

source("Plot_Functions/plot_CV.R")
source("Plot_Functions/block_ckeck_plot.R")

show.metric <-function(X, metric, threshold = NULL){
  if (is.data.frame(X)){
    X <- list(X)
  }
  
  plot.list <- lapply(X, function(df){
    plot.metric(df, metric, threshold)
  })
  
  do.call(grid.arrange, c(plot.list, ncol = length(X)))
  
}

# show.cv.selection <- function(X, threshold = NULL){
#   
#   if (is.data.frame(X)){
#     X <- list(X)
#   }
#   
#   plot.list <- lapply(X, function(df){
#     plot.cv(df, threshold)
#   })
#   
#   do.call(grid.arrange, c(plot.list, ncol = length(X)))
# }
# 
# source("Plot_Functions/plot_IQR.R")
# 
# show.iqr.selection <- function(X, threshold = NULL){
#   
#   if (is.data.frame(X)){
#     X <- list(X)
#   }
#   
#   plot.list <- lapply(X, function(df){
#     plot.iqr(df, threshold)
#   })
#   
#   do.call(grid.arrange, c(plot.list, ncol = length(X)))
# }
# 
# source("Plot_Functions/plot_freqCut.R")
# 
# show.freqCut.selection <- function(X, threshold = NULL){
#   
#   if (is.data.frame(X)){
#     X <- list(X)
#   }
#   
#   plot.list <- lapply(X, function(df){
#     plot.freqCut(df, threshold)
#   })
#   
#   do.call(grid.arrange, c(plot.list, ncol = length(X)))
# }
# 
# source("Plot_Functions/plot_uniqueCut.R")
# 
# show.uniqueCut.selection <- function(X, threshold = NULL){
#   
#   if (is.data.frame(X)){
#     X <- list(X)
#   }
#   
#   plot.list <- lapply(X, function(df){
#     plot.uniqueCut(df, threshold)
#   })
#   
#   do.call(grid.arrange, c(plot.list, ncol = length(X)))
# }
# 
source("Plot_Functions/plot_bx_plt.R")

show.var.distr <- function(X){

  if (is.data.frame(X)){
    X <- list(X)
  }

  plot.list <- lapply(X, function(df){
    plot.bx.plt(df)
  })

  do.call(grid.arrange, c(plot.list, ncol = length(X)))
}



