library(network)
library(GGally)
library(igraph)

source("Network_functions/get_elist.R")
source("Network_functions/get_nodes.R")

create.network <- function(ntw, # Adjency matrix from mixOmics::network() function
                           palette, # Coloer to be attributed to each nodes 
                           size.min = NULL, # minimum node degree to be displayed
                           correspondance = NULL,
                           cluster = NULL
                           ){
  e.list <- get.elist(ntw = ntw, correspondance = correspondance)
  
  nodes <- get.nodes(e.list)
  
  p <- ggnet2(e.list,
         color = nodes$Type, palette = palette,
         size = nodes$Degree,
         size.min = size.min,
         edge.lty = 'Edge.type',
         edge.color = 'Interaction.type.num',
         edge.size = 'Value.abs',
         label = T, 
         label.size = 3
  )
  
  if (!is.null(cluster)){
    # Applying the cluster function on a igraph object
    net.igr <- graph_from_edgelist(as.matrix(e.list[, c('from', 'to')]), directed = F) # creat igrpah object from edgelist
    clusterd.net <- cluster_louvain(net.igr) # applying clusturing algorithm
    
    groups <- data.frame( # extracting groups form igraph object
      name = clusterd.net$names,
      group = clusterd.net$membership
    )
    
    nodes <- nodes %>% 
      left_join(groups, by = 'name')
    
    num.group <- unique(groups$group)
    
    warning(length(num.group), " groups have been found")
    
    # Saving each group on a different graph
    graph.list <- list()
    net <- list()
    net.set <- list()
    for (g in num.group){
      # Creating a distinct elist and node dataframe for each group
      new.e.list <- e.list %>% filter(from %in% nodes$name[nodes$group == g] & to %in% nodes$name[nodes$group == g])
      new.nodes <- get.nodes(new.e.list)
      
      net.set[[g]] <- list(e.list = new.e.list, 
                       nodes = new.nodes)
      
      graph.list[[g]] <- tryCatch({
        ggnet2(new.e.list,
               color = new.nodes$Type, palette = palette,
               size = new.nodes$Degree,
               size.min = size.min,
               edge.lty = 'Edge.type',
               edge.color = 'Interaction.type.num',
               edge.size = 'Value.abs',
               label = T, 
               label.size = 3
        )
      }, error = function(e) {
        ggnet2(new.e.list,
               color = new.nodes$Type, palette = palette,
               size = new.nodes$Degree,
               size.min = size.min,
               label = T, 
               label.size = 3)
      })
    }
    
    # Creating a unique set of networks
    e.list <- lapply(net.set, function(df){
      df[[1]]
    })
    
    nodes <- lapply(net.set, function(df){
      df[[2]]
    })
    
    big.e.list <- do.call(rbind, e.list)
    big.nodes <- do.call(rbind, nodes)
    
    
    p <- ggnet2(big.e.list,
                color = big.nodes$Type, palette = palette,
                size = big.nodes$Degree,
                size.min = size.min,
                edge.lty = 'Edge.type',
                edge.color = 'Interaction.type.num',
                edge.size = 'Value.abs',
                label = T, 
                label.size = 3
    )
    
    return(list(plot = graph.list,
                net = net.set, 
                global.net = p))
  }
  
  return(list(plot = p,
              net = list(e.list = e.list, 
                         nodes = nodes)))
}