extrair_ipc_linha <- function(linha){
  library(rebus)
  library(stringr)
  
  codigo_de_interesse <- capture(START %R% WRD %R%
                       repeated(DGT, lo =2, hi = 2) %R%
                       WRD %R%
                       repeated(DGT, lo = 4, hi = 4)) %R%
              "000000"
  pontuacao <- or(SPC, ";", capture = FALSE)
  palavra <- any_char(1,50)
  significado_pat <- "\t" %R% capture(palavra)

  if (str_detect(linha, pattern = codigo_de_interesse)== TRUE){
    ipc <- str_match(linha, pattern = codigo_de_interesse)[1,2]
    ipc <- str_sub(ipc, 1,8)
    significado <- str_match(linha, significado_pat)[1,2]
    x <- c(ipc, significado)
  }else{x = NULL}
  
  return(x)
  
}