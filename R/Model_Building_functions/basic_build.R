library(mixOmics)

builder <- function(X, Y, tune, nzv, design, max.comp, dist = "all", core.workers){  
  if (tune == 'splsda'){
    model <- block.splsda(X = X,
                          Y = Y,
                          ncomp = max.comp,
                          design = design,
                          scale = F,
                          near.zero.var = nzv)
  } else {
    model <- block.plsda(X = X,
                         Y = Y,
                         ncomp = max.comp,
                         design = design,
                         scale = F,
                         near.zero.var = nzv)
  }
}