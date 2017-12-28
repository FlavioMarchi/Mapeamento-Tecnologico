juntar_espacenetCSV <- function() {
  
  flag <- TRUE
  i <- 0
  while (flag == TRUE){
    
    if (i == 0){
      arquivo <- "results.csv"
    }
    else if (i > 0) {
      arquivo <- str_c("results ","(",i,").csv")}
      caminho <- file.path(getwd(), "Dados", arquivo)
    if (file.exists(caminho)){
      if (i == 0) {dados <- read_csv (file = caminho, 
                                      skip = 0, col_types =cols(.default = "c"))}
      else {
        dados02 <- read_csv(file = caminho, 
                            skip = 0, col_types = cols(.default = "c"))
        dados <- bind_rows (dados, dados02)
      }
    } else {flag <- FALSE}
    i <- i + 1
    
  }
  
  return(list (c(i-2),
               dados))
}