library(dplyr)

get.correspondance.table <- function(X, corsp.tab){
  Types <- names(corsp.tab)
  
  if (any(is.na(match(names(corsp.tab), names(X))))){
    warning("There is a missmatch name between block list (X) and corsp.tab")
  }
  
  for (tab in seq_along(corsp.tab)){
    corsp.tab[[tab]] <- corsp.tab[[tab]] %>% mutate(c = Types[tab])
    names(corsp.tab[[tab]]) <- c("a", "b", "c")
  }
  return(bind_rows(corsp.tab))
}
