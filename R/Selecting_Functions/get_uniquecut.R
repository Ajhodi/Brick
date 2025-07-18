get.uniqueCut.list <- function(df, # a dataframe object
                               uniqueCut) # a float/int range = [0.0, 1.0]
{

  if (is.null(uniqueCut)){
    return(NULL)
  }

  V <- sapply(df, function(col){
    t <- table(col)
    sum(t == 1)/length(t)
  })
  qt <- quantile(V, na.rm = T, probs = uniqueCut)
  selected_V <- V[V >= qt] %>% na.omit()

  return(names(selected_V))
}
