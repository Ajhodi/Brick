# Main function #----

## Spectral Divison #----
spectral.division <- function(X, fx){
  return(X / fx)
}


## MA Normalizer #----



# Sub functons #----

## TIC #----

tic.norm <- function(X){
  apply(X, MARGIN = 1, function(X){
    sum(X)
  })
  # prop.table(X, margin = 1)
}

## MSTUS #----
mstus.norm <- function(X){
  return(sum(X))
}

## VECT #----

vect.norm <- function(X){
  apply(X, MARGIN = 1, function(X){
    (sum(X^2))^(1/2)
  })
}

## Mean ##----

mean.norm <- function(X){
  apply(X, MARGIN = 1, function(X){
    mean(X)
  })
}

## Median ##----

median.norm <- function(X){
  apply(X, MARGIN = 1, function(X){
    median(X)
  })
}

## MAD ##----

mad.norm <- function(X){
  apply(X, MARGIN = 1, function(X){
    median(abs(X - median(X)))
  })
}

# res.tmp <- spectral.division(tmpX, tic.norm(tmpX))

