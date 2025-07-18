library(dplyr)


make_it_match <- function(Y, df, on = 'Month'){
  Y <- Y %>%
    data.frame() %>%
    rename(!!on := 1)
  
  df <- df %>%
    inner_join(Y, by = on, relationship = "many-to-many") %>%
    mutate(Order = match(!!sym(on), Y[[on]])) %>%
    arrange(Order) %>%  
    dplyr::select(-c(Order)) %>%
    distinct(Samples, .keep_all = TRUE)
  
  return(df)
}
