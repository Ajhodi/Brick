library(readr)
library(dplyr)

load.ex.block <- function(folder = 1){
  
  if (folder == 1){
    return(
      list(X = list(NEG = read.csv(paste0("../data/example/", folder, "/NEG.csv")),
                    POS = read.csv(paste0("../data/example/", folder, "/POS.csv")),
                    Taxon = read.csv(paste0("../data/example/", folder, "/Taxon.csv"))),
           Y = read.csv(paste0("../data/example/", folder, "/Y.csv")) %>% as.vector() %>% unlist())
    )
  }
  
  if (folder == 2){
    return(
      list(X = list(ENV = read.csv(paste0("../data/example/", folder, "/Env.csv")),
                    Metabo = read.csv(paste0("../data/example/", folder, "/Metabo.csv")),
                    OTU = read.csv(paste0("../data/example/", folder, "/OTU.csv"))),
           Y = read.csv(paste0("../data/example/", folder, "/Y.csv")) %>% as.vector() %>% unlist())
    )
  }
  
  if (folder == 3){
    return(
      list(X = list(PHY_EABX = read.csv(paste0("../data/example/", folder, "/PHY_EABX.csv")),
                    PHYSICO = read.csv(paste0("../data/example/", folder, "/PHYSICO.csv")),
                    POS = read.csv(paste0("../data/example/", folder, "/POS.csv")),
                    NEG = read.csv(paste0("../data/example/", folder, "/NEG.csv")),
                    PAM = read.csv(paste0("../data/example/", folder, "/PAM.csv"))),
           Y = read.csv(paste0("../data/example/", folder, "/Y.csv")) %>% as.vector() %>% unlist())
    )
  }
}

