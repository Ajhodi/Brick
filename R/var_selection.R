# Libraries #----
library(dplyr)
library(caret)
library(parallel)
library(foreach)
library(doParallel)
library(foreach)
library(moments)
library(mixOmics)
library(RVAideMemoire)
library(fpc)
library(stats)

# Soucrces #----
source("Selecting_Functions/get_ch.R")
source("Selecting_Functions/get_cv.R")
source("Selecting_Functions/get_freqcut.R")
source("Selecting_Functions/get_iqr.R")
source("Selecting_Functions/get_kurt.R")
source("Selecting_Functions/get_kw.R")
source("Selecting_Functions/get_uniquecut.R")
source("Selecting_Functions/get_vip.R")
source("Selecting_Functions/get_rda.R")

var.selection <- function(X, # A list of dataframe
                          Y, # A response vector (character)
                          cv.threshold = NULL, # percentage of variables to keep based on CV
                          IQR.threshold = NULL, # percentage of variables to keep based on IQR
                          freqCut = NULL,
                          uniqueCut = NULL,
                          kruskal = FALSE,
                          ch.threshold = NULL,
                          kurt.threshold = NULL,
                          vip.threshold = NULL,
                          RDA = F,
                          core.workers = NULL # number of core to mobilise for multicor processing
                          )
  {

  tmp <- lapply(X, function(df){
    df %>% dplyr::select(where(is.character)) %>%
      names()
  })

  # Initializing cluster
  cl <- NULL
  if (!is.null(core.workers)){
    cl <- makeCluster(core.workers)
    registerDoParallel(cl)
  }

  results <- lapply(X, function(df){

    # Get list of variables from every conditions
    selected_CV <- get.cv.list(df, cv.threshold)
    selected_IQR <- get.IQR.list(df, IQR.threshold)
    selected_KW <- get.kw.list(df, Y = Y, kruskal)
    selected_freqCut <- get.freqCut.list(df, freqCut)
    selected_uniqueCut <- get.uniqueCut.list(df, uniqueCut)

    selected_ch <- get.ch.list(df, Y = Y, ch.threshold = ch.threshold)
    selected_kurt <- get.kurt.list(df, kurt.threshold)
    selected_vip <- get.vip.list(df, Y = Y, vip.threshold = vip.threshold)

    return(list(X_CV = selected_CV,
                X_IQR = selected_IQR,
                X_kw = selected_KW,
                X_fc = selected_freqCut,
                X_uc = selected_uniqueCut,
                X_ch = selected_ch,
                X_kurt = selected_kurt,
                X_vip = selected_vip
                ))
  })

  # Rearranging outup into list of list
  selected_CV <- lapply(results, `[[`, "X_CV")
  selected_IQR <- lapply(results, `[[`, "X_IQR")
  selected_KW <- lapply(results, `[[`, "X_kw")
  selected_freqCut <- lapply(results, `[[`, "X_fc")
  selected_uniqueCut <- lapply(results, `[[`, "X_uc")
  selected_ch <- lapply(results, `[[`, "X_ch")
  selected_kurt <- lapply(results, `[[`, "X_kurt")
  selected_vip <- lapply(results, `[[`, "X_vip")

  if (!is.null(cl)){
    stopCluster(cl)
  }

  return_list <- list(X_CV = selected_CV,
                      X_IQR = selected_IQR,
                      X_kw = selected_KW,
                      X_fc = selected_freqCut,
                      X_uc = selected_uniqueCut,
                      X_ch = selected_ch,
                      X_kurt = selected_kurt,
                      X_vip = selected_vip
                      )
  if (!isFALSE(RDA)){
    return_list$X_rda <- RDA.selection(X, rda = RDA, ncore = core.workers)
    names(return_list$X_rda) <- names(X)
  }

  # Remove NULL matricies
  return_list <- return_list[sapply(return_list, function(x) !all(sapply(x, is.null)))]

  # Put back non-numerical values of each block
  return_list <- lapply(return_list, function(df){
    for (i in seq_along(tmp)){
      df[[i]] <- unique(c(df[[i]], tmp[[i]]))
    }
    return(df)
  })

  return(return_list)
}

