library(dplyr)
library(ggvenn)
library(gridExtra) 

shrink.block <- function(X, # list of dataframe
                        reduced_X, # list of n vectors (n = number of block) of the variables to be kept
                        plot = T
                             )
  {
  
  keep_X <- list()
  venn.diagram.list <- list()
  
  if (length(reduced_X) == 1){
    keep_X <- reduced_X[[1]]
  } else {
    for (i in 1:length(X)){
      # Regrouping variables from de same block
      block.elements <- lapply(reduced_X, function(df){
        df[[i]]
      })
      keep_X[[i]] <- Reduce(intersect, Filter(Negate(is.null), block.elements)) # Regrouping common elements 
      if (isTRUE(plot) && length(reduced_X) > 1){
        venn.diagram.list[[i]] <- ggvenn(block.elements, show_percentage = F, stroke_color = 'white',
                                         fill_color = c("blue", "yellow", "green", "red", "cyan", "tomato", "orange", "purple"))
      }
    }
  }
  # Selecting varaibles from the originals dataframes
  if (length(keep_X) != 0){
    for (i in 1:length(X)){
      X[[i]] <- X[[i]] %>%
        dplyr::select(any_of(keep_X[[i]] %>% na.omit()))
    }
  }
  
  if (isTRUE(plot) && length(reduced_X) > 1){
    do.call(grid.arrange, c(venn.diagram.list, ncol = length(X)))
  }
  return(X)
}
