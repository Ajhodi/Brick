library(mixOmics)

tunning <- function(basic.model, X, Y, tune, nzv, design, dist = "all", max.comp, core.workers, progressBar){
  
  tune.TCGA <- NULL
  list.keepX <- NULL
  
  if (tune == 'splsda'){
    BPPARAM <- BiocParallel::SnowParam(workers = core.workers)
    tune.TCGA <- tune.block.splsda(X = X, 
                                   Y = Y,    
                                   design = design, 
                                   folds = 4, 
                                   ncomp = max.comp,
                                   nrepeat = 2, 
                                   near.zero.var = nzv, 
                                   dist = dist,
                                   scale = F,
                                   BPPARAM = BPPARAM,
                                   progressBar = progressBar) 
  }
  
  if (!is.null(tune.TCGA)){
    final.model <- block.splsda(X = X,
                                Y = Y,
                                ncomp = max.comp,
                                # keepX = list.keepX,
                                keepX = tune.TCGA$choice.keepX,
                                design = design,
                                scale = F,
                                near.zero.var = nzv)
  }
  
  else {final.model <- block.plsda(X = X,
                                   Y = Y,
                                   ncomp = max.comp, 
                                   design = design,
                                   scale = F,
                                   near.zero.var = nzv)
  }
  
  return(final.model)
}