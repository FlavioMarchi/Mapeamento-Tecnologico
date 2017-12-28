importar_dados <- function(base, tipo_arquivo){
  source("limpar_dados.R")
  source("juntar_espacenetXLS.R")
  source("juntar_espacenetCSV.R")
  source("juntar_patentscope.R")
  
  # Deve-se escolher a base e o tipo de arquivo desejado
  # Ambos são strings
  # base = Espacenet ou
  #        Patentscope
  

if (base == "Espacenet"){
    nome <- paste("results.",tipo_arquivo, sep = "")
    caminho <- file.path(getwd(),"Dados",nome)

    if (tipo_arquivo == "xls"){
    # dados <- read_excel(caminho, skip = 4,col_types = "text")
      dados <- juntar_espacenetXLS()[[2]]
      }
    else if (tipo_arquivo == "csv"){
    # dados <- read_csv(caminho, skip = 0, col_types = cols(.default = "c"))
      dados <- juntar_espacenetCSV()[[2]]
    } else {
      stop("Tipo de arquivo não é .csv ou .xls")
    }
    #Descartando colunas esparsas
    dados <- dados %>% 
      select(-c(11:24))
    dados <- dados %>%
      select(-c(7))
    # Renomeando as colunas
    names(dados) <- c("title", "pub_n", "pub_date", "inventor",
                      "applicant", "ipc", "app_n",
                      "app_date", "priority_n")
    
    # Selecionando as colunas comuns
    dados <- dados[, c("title", "pub_n", "pub_date",
                       "inventor", "applicant", "ipc")]
} 
else if (base == "Patentscope"){
  # nome <- paste("resultList.","xls", sep = "")
  # caminho <- file.path(getwd(),"Dados",nome)
  # dados <- read_excel(caminho, skip = 2,col_types = "text")
  dados <- juntar_patentscope()[[2]]
  # Removendo coluna FP Image
  dados <- dados[,1:7]
  # Renomeando as colunas, conforme o padrão adotado
  nomes_col <- c("pub_n","pub_date"  , "title"   , "priority_n", 
                 "ipc"  , "applicant", "inventor")
  names(dados) <-nomes_col
  
  # Removendo a coluna priority_n, há muitos faltantes, 46%
  dados <- dados[,-4]
  
  # Selecionando as colunas comuns
  dados <- dados[, c("title", "pub_n", "pub_date",
                       "inventor", "applicant", "ipc")]
  
}
  else{stop("Nome da base está incorreto.")}

#dados <- dados[, c("title", "pub_n", "pub_date",
                    # "inventor", "applicant")]
dados <- limpar_dados(dados, base)
return(dados)
  
}