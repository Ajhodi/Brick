library(IMIFA)
library(compositions)
library(foreach)
library(parallel)

# Intrablock scaling #----

## Functions #----

source("match_blocks.R")
source("Scaling_Functions/intrascaling.R")

## Main #----

intra.scale <- function(X, Y, method = 1, core.workers = NULL) {
  
  if (is.data.frame(X)){
    X <- list(X)
  }
  
  if (!is.null(core.workers)){
    cl <- makeCluster(core.workers)
    registerDoParallel(cl)
  }
  tmp <- names(X)
  
  # if (is.list(X) && !is.data.frame(X)){
  #   X <- foreach(i = seq_along(X), .packages = c('dplyr'), .export = c('Y', 'make_it_match')) %dopar% {
  #     make_it_match(Y, X[[i]])
  #   }
  # } 
  
  if (method == 1 && !is.data.frame(X)){
    message("scale has been applied")
    X <- foreach(i = seq_along(X), .packages = c('dplyr', 'IMIFA', 'tibble'),
                 .export = c('Y', 'X','classic.scale')) %dopar% {
          classic.scale(X[[i]], Y)
    } 
  }
  
  if (method == 2 && !is.data.frame(X)){
    message("log transformation and pareto scale have been applied")
    X <- foreach(i = seq_along(X), .packages = c('dplyr', 'IMIFA', 'tibble'),
                 .export = c('Y', 'X','log.pareto')) %dopar% {
          log.pareto(X[[i]], Y)
    } 
  } 

  if (method == 3 && !is.data.frame(X)){
    message("Median scaling, cube root transformation and pareto scale have been applied")
    X <- foreach(i = seq_along(X), .packages = c('dplyr', 'IMIFA', 'tibble'),
                 .export = c('Y', 'med.cube.pareto')) %dopar% {
      med.cube.pareto(X[[i]], Y)
    } 
  } 

  if (method == 4 && !is.data.frame(X)){
    message("Centered log ratio transform has been applied")
    X <- foreach(i = seq_along(X), .packages = c('dplyr', 'IMIFA', 'tibble', 'compositions'),
                 .export = c('Y', 'clr.')) %dopar% {
      clr.(X[[i]], Y)
    } 
  } 

  if (isFALSE(method) && !is.data.frame(X)){
    message("No scaling nor transformation has been applied")
    X <- foreach(i = seq_along(X), .packages = c('dplyr', 'IMIFA', 'tibble', 'compositions'),
                 .export = c('Y', 'no.scale')) %dopar% {
      no.scale(X[[i]], Y)
    } 
  } 

  if(!is.null(core.workers)){
    stopCluster(cl)
  }
  names(X) <- tmp
  
  if (length(X) > 1 ){
    return(X)
  } else {
    return(X[[1]])
  }
}

 # Intrablock scaling #----

## Functions #----
source("Scaling_Functions/interscaling.R")

## Main #----

inter.scale <- function(X, method = FALSE, core.workers = 1){
  tmp <- names(X)
  cl <- makeCluster(core.workers)
  
  if (method == 1){
    message("Soft-Block Scaling has been applied")
    X <- foreach(i = seq_along(X), .export = 'SBS') %dopar% {
      SBS(X[[i]])
    }
  }
  
  if (method == 2){
    message("Hard-Block Scaling has been applied")
    X <- foreach(i = seq_along(X), .export = 'HBS') %dopar% {
      HBS(X[[i]])
    }
  }
  
  if (method == 3){
    message("Super Hard-Block Scaling has been applied")
    X <- foreach(i = seq_along(X), .export = 'SHBS') %dopar% {
      SHBS(X[[i]])
    }
  }
  
  if (method == 4){
    message("Soft-Block Variance Scaling")
    X <- foreach(i = seq_along(X), .export = 'SBVS') %dopar% {
      SBVS(X[[i]])
    }
  }
  
  if (method == 5){
    message("Hard-Block Variance Scaling has been applied")
    X <- foreach(i = seq_along(X), .export = 'HBVS') %dopar% {
      HBVS(X[[i]])
    }
  }
  
  if (method == 6){
    message("Super Hard-Block Variance Scaling")
    X <- foreach(i = seq_along(X), .export = 'SHBVS') %dopar% {
      SHBVS(X[[i]])
    }
  }
  
  stopCluster(cl)
  names(X) <- tmp
  return(X)
}
