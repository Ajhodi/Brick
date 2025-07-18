library(mixOmics)

source("Model_Building_functions/basic_build.R")
source("Model_Building_functions/get_parameters.R")
source("Model_Building_functions/tunn.R")

diablo.model <- function(X, Y, tune = 'plsda', nzv = TRUE, design = NULL, max.comp = 2, core.workers = 1, progressBar = F){
  
  if (is.null(design)){ # default design matrix
    design <- matrix(0.1, ncol = length(X), nrow = length(X), 
                    dimnames = list(names(X), 
                                    names(X)))
    
    diag(design) = 0
  }
  
  basic.model <- builder(X, Y, tune, nzv, design, max.comp, core.workers) # Create a basic model
  
  perf.parameter <- get.parameters(basic.model, X, progressBar, core.workers, max.comp) # Evaluation of the basic model to get hyperparemter (ncomp and dist)
  
  final.model <- tunning(basic.model, X, Y, tune, nzv, design, # Final model
                         dist = perf.parameter$dist, 
                         max.comp =  perf.parameter$ncomp, 
                         core.workers, progressBar)
  
  return(list(model = final.model,
              Parameters = perf.parameter))
}

             
               
