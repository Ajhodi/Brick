get.ch.list <- function(X, Y,
                        plow = 0.05,
                        psup = 0.95,
                        ch.threshold
)
{

  if (is.null(ch.threshold)){
    return(NULL)
  }

  X <- X %>%
    dplyr::select(where(length(unique(.))) >= length(Y)) %>%
    dplyr::select(where(is.numeric)) %>%
    select_if(function(col) {
      outliers <- which(col < quantile(col, plow) | col > quantile(col, psup))
      length(outliers) > 0  # Remove outlayers
    })

  ch.list <- sapply(X, function(col){
    kmeans_results <- kmeans(col, centers = length(unique(Y)))
    cluster.stats(col, kmeans_results$cluster)$ch
  })

  qt <- quantile(ch.list, na.rm = T, probs = ch.threshold)
  selected <- ch.list[ch.list >= qt]

  return(names(selected))
}
