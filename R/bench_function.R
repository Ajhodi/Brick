library(foreach)
library(doParallel)

source("var_selection.R")
source("shrink_block.R")
source("scaling_process.R")
source("model_maker.R")

bench <- function(X, Y,
                  selection = c("cv.threshold", "IQR.threshold"),
                  range.x = seq(0.1, 0.9, 0.2),
                  range.y = seq(0.1, 0.9, 0.2),
                  intra.scale = 1,
                  inter.scale = NULL,
                  tune = 'plsda',
                  ncore = NULL) {

  # Initializing cluster
  if (!is.null(ncore)) {
    cl <- makeCluster(ncore)
    registerDoParallel(cl)
  }

  results <- foreach(x = 1:length(range.x), .combine = 'rbind', .export = c('X', 'Y', 'selection',
                                                                            'range.x', 'range.y')) %dopar% {
    source("var_selection.R")
    source("shrink_block.R")
    source("scaling_process.R")
    source("model_maker.R")
                                                                              
    args_list <- list(
      X = X, 
      Y = as.vector(Y),
      cv.threshold = NULL,
      IQR.threshold = NULL,
      freqCut = NULL,
      uniqueCut = NULL,
      kruskal = NULL,
      ch.threshold = NULL,
      kurt.threshold = NULL,
      vip.threshold = NULL,
      RDA = NULL,
      core.workers = NULL
    )

    local_error <- rep(NA, length(range.y))
    local_variable <- rep(NA, length(range.y))

    for (y in 1:length(range.y)) {
      # Selection
      filtered_args <- args_list[c("X", "Y", selection)]
      filtered_args[[selection[1]]] <- range.x[x]
      filtered_args[[selection[2]]] <- range.x[y]
      
      reduced <- do.call(var.selection, filtered_args)
      
      X.r <- shrink.block(X, reduced, plot = F)
      local_variable[y] <- do.call(sum, lapply(X.r, ncol))
      # Scaling
      X.s <- intra.scale(X.r, Y, method = intra.scale, core.workers = NULL)
      if (!is.null(inter.scale)){
        X.s <- inter.scale(X.s, method = inter.scale, core.workers = NULL)
      }
      # Model
      local_error[y] <- tryCatch({
        model.2 <- diablo.model(X.s, Y, tune = tune, nzv = F, max.comp = 4)
        error_rate <- model.2$Parameters$global.min.error.rate
      }, error = function(e) {
        NA
      })
    }
    return(c(local_variable, local_error))
  }
  row.names(results) <- range.x
  colnames(results) <- rep(range.y, 2)
  # Stop cluster
  if (!is.null(ncore)) {
    stopCluster(cl)
  }

  return(list(error.rate = results[, 1:length(range.y)],
              number.variable = results[, (length(range.y) + 1):(length(range.y) * 2)]))
}


