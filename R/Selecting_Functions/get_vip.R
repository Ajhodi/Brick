get.vip.list <- function(df,
                         Y,
                         vip.threshold)
{

  if (is.null(vip.threshold)){
    return(NULL)
  }

  df <- df %>% dplyr::select(where(is.numeric))
  model <- plsda(df, Y, scale = F) # Create model
  vip.list <- PLSDA.VIP(model)$tab # Get VIP

  name.list <- row.names(vip.list)
  vip.list <- vip.list$VIP
  names(vip.list) <- name.list

  qt <- quantile(vip.list, probs = vip.threshold)
  selected <- vip.list[vip.list >= qt]
  return(names(selected))
}
