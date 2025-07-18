library(stats)
library(tibble)

cluster <- function(net, k = 2, palette, size.min = 0){

  e.list <- net[[1]] 
  nodes <- net[[2]]
  
  hc <- hclust(dist(nodes %>% 
                      column_to_rownames(var = 'name') %>%
                      dplyr::select(Degree)), 
               method = "centroid")
  
  clusters <- data.frame(cluster = cutree(hc, k = 2)) %>% 
    rownames_to_column(var = 'name') %>%
    mutate(cluster = as.factor(cluster))
  
  e.list <- e.list %>%
    left_join(clusters, by = c("from" = "name"))
  
  nodes <- nodes %>%
    left_join(e.list %>% dplyr::select(from, cluster) %>% distinct(), by = c("name" = "from")) %>%
    left_join(e.list %>% dplyr::select(to, cluster) %>% distinct(), by = c("name" = "to")) %>%
    mutate(cluster = coalesce(cluster.x, cluster.y)) %>%
    dplyr::select(-c(cluster.x, cluster.y)) %>%
    distinct(name, .keep_all = TRUE)
  
  plot <- ggnet2(e.list, 
                 shape = nodes$Type,
                 color = nodes$cluster, palette = palette,
                 size = nodes$Degree,
                 size.min = size.min,
                 edge.lty = 'Edge.type',
                 edge.size = 'Value.abs',
                 label = T, 
                 label.size = 2
  )
  
  return(plot)
}