juntar_espacenet <- function() {
  
  flag <- TRUE
  i <- 0
  while (flag == TRUE){
    
    if (i == 0){
      arquivo <- "results.xls"
    }
    else if (i > 0) {
      arquivo <- str_c("results ","(",i,").xls")}
    # else if (i >= 10){
    #   arquivo <- str_c("results",i,".xls")
    # }
    caminho <- file.path(getwd(), "Dados", arquivo)
    if (file.exists(caminho)){
      if (i == 0) {dados <- read_xls (caminho, skip = 4, col_types = "text")}
     else {
      dados02 <- read_xls(caminho, skip = 4, col_types = "text")
      dados <- bind_rows (dados, dados02)
     }
    } else (flag <- FALSE)
    i <- i + 1

  }

  return(list (c(i-2),
               dados))
}