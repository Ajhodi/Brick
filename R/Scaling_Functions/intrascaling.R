library(tibble)
library(dplyr)

classic.scale <- function(df, Y){
  df <- df %>%
    column_to_rownames(var = "Samples") %>%
    data.frame() %>%
    select_if(is.numeric) %>%
    scale(center = TRUE, scale = TRUE) %>%
    as.data.frame()
}

log.pareto <- function(df, Y){
  df <- df %>%
    column_to_rownames(var = "Samples") %>%
    data.frame() %>%
    select_if(is.numeric) %>%
    log %>%
    pareto_scale() %>%
    as.data.frame()
}

med.cube.pareto <- function(df, Y){
  df <- df %>%
    column_to_rownames(var = "Samples") %>%
    select_if(is.numeric) %>%
    mutate(across(everything(), ~ scale(., center = median(.), scale = TRUE))) %>% # median scaling
    {sign(.) * abs(.)^(1/3)}  %>% # cube root transformation
    pareto_scale() %>%
    as.data.frame()
}

clr. <- function(df, Y){
  df <- df %>%
    column_to_rownames(var = "Samples") %>%
    select_if(is.numeric) %>%
    clr() %>% # Centered log ratio transform
    as.data.frame()
}

no.scale <- function(df, Y){
  df <- df %>%
    column_to_rownames(var = "Samples") %>%
    select_if(is.numeric) %>%
    as.data.frame()
}