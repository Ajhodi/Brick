library(mixOmics)

get.error.rate <- function(model.perf){
  # Get min error value accross each block
  error.rate <- lapply(model.perf[["error.rate"]], min)
  # Average the results
  mean.error <- mean(unlist(error.rate))
  return(mean.error)
}

get.best.distance <- function(model.perf) {
  # Get the min distance for each block
  
  min.dist.list <- lapply(model.perf[["error.rate"]], function(df){
    data.frame(df) %>% 
      summarise(across(everything(), min)) %>% 
      which.min() %>% 
      names()
  })
  # Get the most occurent distance
  dist <- table(unlist(min.dist.list)) %>% 
    which.max() %>% 
    names()
  
  return(dist)
}

get.best.comp <- function(model.perf) {
  # Get the min comp for each block
  min.comp.list <- lapply(model.perf[["error.rate"]], function(df){
    row.names(df) <- row.names(df) %>% rank()
    
    data.frame(df) %>% 
      t() %>% data.frame() %>%
      summarise(across(everything(), min)) %>%
      which.min() %>%
      names() 
  })
  # Get the most occurent comp
  comp <- table(unlist(min.comp.list)) %>% 
    which.max() %>% 
    names()
  
  best.comp <- as.numeric(gsub("X", "", comp))
  best.comp <- ifelse(best.comp == 1, 2, best.comp)
  
  return(best.comp)
}

get.parameters <- function(basic.model, X, progressBar, core.workers, max.comp){
  
  model.perf <- perf(basic.model, 
                     progressBar = progressBar,
                     validation = c("Mfold", "loo"),
                     folds = max.comp, 
                     nrepeat = 2, 
                     dist = "all",
                     cpus = core.workers)
  
  ncomp <- get.best.comp(model.perf)
  
  dist <- get.best.distance(model.perf)
  
  error.rate <- model.perf$error.rate
  
  min.error <- get.error.rate(model.perf)
  
  return(list(ncomp = ncomp,
              dist = dist,
              error.rate = error.rate,
              global.min.error.rate = min.error))
}

