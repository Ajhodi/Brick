library(progress)

RDA.selection <- function(X, rda = F, ncore = NULL) {
  
  if (isFALSE(rda)){
    return(NULL)
  }
  
  # Progress bar
  pb <- progress_bar$new(
    total = length(X),
    clear = FALSE,
    width = 60,
    format = "RDA selection [:bar] :percent | Temps restant :eta") 
  
  if (!is.null(ncore)){
    cl <- parallel::makeCluster(ncore)
    doParallel::registerDoParallel(cl)
  }
  
  best_PC_list <- list()
  
  for (i in 1:length(X)){
    X[[i]] <- X[[i]] %>% 
      select_if(~ is.numeric(.)) %>%
      data.frame()
  }
  
  for (i in 1:length(X)) {
    
    # Character selection
    take_best_PC_name <- function(str) {
      return(gsub("\\+\\s*", "", str))
    }
    
    best_PC_sublist <- foreach(j = 1:length(X), .packages = c('vegan')) %dopar% {
      if (i != j) {
        data <- X[[i]]
        x_ <- X[[j]]
        
        res_RDA <- vegan::rda(x_ ~ ., data = data)
        res_RDA0 <- vegan::rda(x_ ~ 1, data = data)
        
        step.res <- tryCatch({
          vegan::ordiR2step(res_RDA0,
                            scope = formula(res_RDA),
                            direction = "both", 
                            R2scope = FALSE, 
                            trace = TRUE)
        }, error = function(e) {
          message("Erreur lors de l'exécution de ordiR2step : ", e$message)
          return(NULL)
          if (!is.null(ncore)){
            parallel::stopCluster(cl)
          }
        })
        
        if (!is.null(step.res)) {
          best_PC <- step.res$anova
          best_PC_names <- rownames(best_PC)
          
          best_PC_names_cleaned <- sapply(best_PC_names, take_best_PC_name)
          return(unique(best_PC_names_cleaned))
        }
      }
    }
    
    best_PC_list[[i]] <- unlist(best_PC_sublist)
    pb$tick()
  }
  
  if (!is.null(ncore)){
    parallel::stopCluster(cl)
  }
  
  return(best_PC_list)
}