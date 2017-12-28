limpar_dados <- function(dados, base){
  source("nomear_paises.R")
library(rebus)
  
  
  
  if (base == "Espacenet"){
# mudando a formatação de data de publicação para apenas o ano.
    dados <- dados %>% 
      mutate(year = as.integer(str_sub(pub_date, start = 1, end = 4))) %>% 
# mudando a informação do número da patente para o país de depósito
      mutate(country = str_sub(pub_n, start = 1, end = 2))
# eliminar os paises que aparecerem em applicants
    pattern1 <- "\\[" %R% repeated(WRD, lo = 2, hi = 2) %R% "\\]"
    dados$applicant <- str_replace_all(dados$applicant, pattern = pattern1, replacement = "")
    
    
    }
    
    else if (base == "Patentscope") {
      dados <- dados %>% 
      mutate(year = as.integer(str_sub(pub_date, start = -4, end = -1))) %>% 
# mudando a informação do número da patente para o país de depósito
      mutate(country = str_sub(pub_n, start = 1, end = 2)) 
    }
  else {stop("Nome da base não está correto.")}
  
# Formatando nomes dos paises
  dados$country <- nomear_paises(dados$country)
# Formatando applicants
  dados$applicant <- str_trim(dados$applicant)
# Eliminando ";" de ipc
  dados$ipc <- str_replace_all(dados$ipc, pattern = " ", replacement = "")
  
  return(dados)
  
  
} 
 