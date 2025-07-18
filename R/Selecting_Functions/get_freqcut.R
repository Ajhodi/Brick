get.freqCut.list <- function(df, # a dataframe object
                             freqCut) # a float/int range = [0.0, 1.0]
{

  if (is.null(freqCut)){
    return(NULL)
  }

  V <- sapply(df, function(col){
    t <- table(col) %>% sort(decreasing = T)
    t[1]/t[2]
  })
  qt <- quantile(V, na.rm = T, probs = freqCut)
  selected_V <- V[V <= qt] %>% na.omit()

  return.list <- names(selected_V)

  return(sapply(return.list, function(x) strsplit(x, "\\.")[[1]][1]))
}
