get.kw.list <- function(df, # a dataframe object
                        Y, # A response vector (character)
                        kruskal
)
{

  if (isFALSE(kruskal)){
    return(NULL)
  }

  # Performing a classic Kruskall Wallis test
  V <- sapply(names(df), function(col){
    frml <- as.formula(paste(paste0("`",col,"`"), "~ Y")) # create the formula as formula = col ~ Y
    kw <- kruskal.test(frml, data = df)
    if (!is.na(kw$p.value) && !is.null(kw) && kw$p.value < 0.05){ # selecting the variables based on theire kw reseults (p<0.05)
      return(col)
    }
  })

  return(as.vector(unlist(V))) # reurns a vector of names (characters)
}
