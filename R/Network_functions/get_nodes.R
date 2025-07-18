library(dplyr)
library(tidyr)
library(tibble)

get.nodes <- function(e.list
                      ){
  nodes <- data.frame(name = unique(c(e.list$from, e.list$to))) %>%
    mutate(id = 0:(length(name)-1)) %>%
    left_join(e.list %>% dplyr::select(from, Source.type) %>% distinct(), by = c("name" = "from")) %>%
    left_join(e.list %>% dplyr::select(to, Target.type) %>% distinct(), by = c("name" = "to")) %>%
    mutate(Type = coalesce(Source.type, Target.type)) %>% 
    dplyr::select(name, id, Type) %>%
    left_join(data.frame(table(c(e.list$from, e.list$to))), by = c("name" = "Var1")) %>%
    rename(Degree = Freq)
  
  return(nodes)
}