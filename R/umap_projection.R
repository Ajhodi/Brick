library(umap)
library(dplyr)
library(ggplot2)

umap.project <- function(X, Samples = NULL){
   
  if (is.null(dim(X))){
    df <- do.call(cbind, X) %>% dplyr::select(where(is.numeric))
  } else {
    df <- X %>% dplyr::select(where(is.numeric))
  }
  
  map <- umap(df)$layout %>%
    as.data.frame()%>%
    rename(UMAP1="V1",
           UMAP2="V2")
  
  if (!is.null(Samples)){
    map$Samples <- X[, `Samples`]
  }
  
  return(map)
  
}

