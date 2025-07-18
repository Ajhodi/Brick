get.kurt.list <- function(df,
                          kurt.threshold
)
{

  if (is.null(kurt.threshold)){
    return(NULL)
  }

  V <- df %>%
    dplyr::select(where(is.numeric)) %>%
    kurtosis()
  qt <- quantile(V, probs = (1-kurt.threshold), na.rm = T)
  selected <- V[V <= qt]

  return(names(selected))
}
