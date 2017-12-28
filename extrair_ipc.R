extrair_ipc_documento <- function(nome_file){
  library(stringr)
  library(rebus)
  source("extrair_ipc_linha")
  
  filepath = file.path(getwd(),"ipc_titulos.txt")
  codigos_ipc <- data.frame(
    ipc <- NULL,
    significado <- NULL
  )
  con = file(filepath, "r")
  
  while ( TRUE ) {
    linha = readLines(con, n = 1)
    if ( length(linha) == 0 ) {
      break
    }else{
      x <- extrair_ipc(linha)
      if(!is.null(x)){
        cod <- data.frame(
          ipc = x[1],
          significado = x[2]
        )
        codigos_ipc <- bind_rows(codigos_ipc, cod)
      }
    }
  }
  close(con)
 
  return(codigos_ipc)
  
}