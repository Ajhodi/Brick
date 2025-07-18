get.IQR.list <- function(df, # a dataframe object
                         IQR.threshold # a float/int range = [0.0, 1.0]
)
{

  if (is.null(IQR.threshold)){
    return(NULL)
  }

  # Selecting a percentage of the best varables based on theire IQR
  V <- sapply(df, function(x) IQR(x, na.rm = TRUE))
  qt <- quantile(V, na.rm = T, probs = IQR.threshold)
  selected_V <- V[V >= qt] %>% na.omit()

  return(names(selected_V)) # reurns a vector of names (characters)
}
