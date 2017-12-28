extrair_ipc_documento <- function(nome_file){
  library(stringr)
  library(rebus)
  source("extrair_ipc_linha.R")
  
  filepath = file.path(getwd(),"Dados", nome_file)
  
  codigos_ipc <- data_frame()
  print(dim(codigos_ipc))
  con = file(filepath, "r")
  while ( TRUE ) {
    linha = readLines(con, n = 1)
    if ( length(linha) == 0 ) {
      break
    }else{
      x <- extrair_ipc_linha(linha)
      
      if(!is.null(x)){
        cod <- data_frame(
          IPC = x[1],
          significado = x[2]
        )
        codigos_ipc <- bind_rows(codigos_ipc, cod)
      }
    }
  }
  close(con)
  
  return(codigos_ipc)
  
}