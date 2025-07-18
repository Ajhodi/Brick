library(tibble)
library(dplyr)


SBS <- function(df){ # Soft-Block Scaling
  Kb <- 1 / (ncol(df))**(1/4)
  return(df/Kb)
}

HBS <- function(df){ # Hard-Block Scaling
  Kb <- 1 / sqrt(ncol(df))
  return(df/Kb)
}

SHBS <- function(df){ # Super Hard-Block Scaling
  Kb <- 1 / ncol(df)
  return(df/Kb)
}

SBVS <- function(df){ # Soft-Block Variance Scaling
  sum.sd <- sum(sapply(df, function(j) sd(j)^2))
  Kb <- (ncol(df))^(1/4) / sqrt(sum.sd)
  return(df/Kb)
}

HBVS <- function(df){ # Hard-Block Variance Scaling 
  sum.sd <- sum(sapply(df, function(j) sd(j)^2))
  Kb <- 1 / sqrt(sum.sd)
  return(df/Kb)
}

SHBVS <- function(df){ # Super Hard-Block Variance Scaling
  sum.sd <- sum(sapply(df, function(j) sd(j)))
  Kb <- 1 / sum.sd
  return(X[[i]]/Kb)
}