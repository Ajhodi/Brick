library(dplyr)
library(tidyr)
library(tibble)
library(stringr)

get.elist <- function(ntw , # Adjency matrix from mixOmics::network() function
                      correspondance # 2d dataframe
                        ){
  # Selecting matrix object from ntw
  ntw <- ntw[sapply(ntw, is.matrix)]
  
  # Creating edge list forme each matrix (one per block)
  e.list <- lapply(ntw, function(mtx){
    data.frame(mtx) %>% 
      rownames_to_column(var = 'from') %>% 
      pivot_longer(cols = colnames(.)[-1],names_to='to',values_to='Value')
  })
  
  e.list <- do.call(rbind, e.list) %>% # Binding all adge list to forme an unique object
    # Formating the dataframe to add extra details (Source.type, Target.type, Edge.type, Value.abs, Interaction.type, Interaction.type.num)
    filter(Value != 0) %>%
    mutate(Source.type = str_extract(from, "[^_]+$")) %>%
    mutate(Target.type = str_extract(to, "[^_]+$")) %>%
    mutate(Edge.type = ifelse(Value > 0, 1, 2),
           Value.abs = abs(Value),
           from = str_remove(from, "_[^_]+$"),
           from = str_remove(from, "^X"),
           Interaction.type = paste(Source.type, Target.type, sep = "_"),
           to = str_remove(to, "_[^_]+$"),
           to = str_remove(to, "^X")
           ) %>%
    mutate(Interaction.type.num = as.numeric(factor(Interaction.type)))
  
  if (!is.null(correspondance)){
    e.list <- e.list %>% 
      
      filter(from %in% correspondance[, 1] & to %in% correspondance[, 1]) %>% # A discuter avec Nicolas
      
      left_join(correspondance, by = c("from" = "a")) %>%
      mutate(from = ifelse(!is.na(b), b, from)) %>%
      dplyr::select(-c(b, c)) %>%
      left_join(correspondance, by = c("to" = "a")) %>%
      mutate(to = ifelse(!is.na(b), b, to)) %>%
      dplyr::select(-c(b, c)) # %>%
      
    e.list <- e.list %>%
      group_by(from, to) %>%
      summarise(
        Value = mean(Value, na.rm = TRUE),
        Edge.type = ifelse(Value > 0, 1, 2),
        Value.abs = abs(Value),
        .groups = 'drop'
      ) 
    
    e.list <- e.list %>%
      mutate(Source.type = correspondance$c[match(e.list$from, correspondance$b)],
             Target.type = correspondance$c[match(e.list$to, correspondance$b)],
             Interaction.type = paste(Source.type, Target.type, sep = "_"),
             Interaction.type.num = as.numeric(factor(Interaction.type)))
    
  }
  return(e.list)
}
