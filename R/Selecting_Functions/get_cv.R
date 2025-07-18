get.cv.list <- function(df, # a dataframe object
                             cv.threshold # a float/int range = [0.0, 1.0]
)
{

  if (is.null(cv.threshold)){
    return(NULL)
  }

  # Selecting a percentage of the best varables based on theire CV
  V <- sapply(df, function(x) sd(x)/mean(x))
  qt <- quantile(V, na.rm = T, probs = cv.threshold)
  selected_V <- V[V >= qt] %>% na.omit()

  return(names(selected_V)) # reurns a vector of names (characters)
}
