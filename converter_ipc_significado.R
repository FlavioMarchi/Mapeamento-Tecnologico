#### Converte o arquivo baixado da WIPO para o padrão adotado e seu significado ####
closeAllConnections()
rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(readr)
library(stringr)
library(countrycode)
library(RColorBrewer)
library(rebus)
library(XLConnect)
library(scales)
library(qdap)
library(tm)
library(wordcloud)



source("importar_dados.R")
source("limpar_dados.R")
source("nomear_paises.R")
source("exportarExcel.R")
source("extrair_ipc_linha.R")
source("extrair_ipc_documento.R")

# x <- read_tsv("ipc_titulos.txt", col_names = FALSE)

lista_documentos_ipc <- c(
                           "EN_ipc_section_A_title_list_20140101.txt",
                          "EN_ipc_section_B_title_list_20140101.txt",
                          "EN_ipc_section_C_title_list_20140101.txt",
                          "EN_ipc_section_D_title_list_20140101.txt",
                          "EN_ipc_section_E_title_list_20140101.txt",
                          "EN_ipc_section_F_title_list_20140101.txt",
                           "EN_ipc_section_G_title_list_20140101.txt",
                          "EN_ipc_section_H_title_list_20140101.txt"
                          )

codigos_ipc_sig <- data_frame()

for (i in 1:length(lista_documentos_ipc)){
  print(i)
  x <- extrair_ipc_documento(lista_documentos_ipc[i])
  print(dim(x))
  print(dim(codigos_ipc_sig))
  codigos_ipc_sig <- bind_rows(codigos_ipc_sig,x)
  print(dim(codigos_ipc_sig))
}

caminho <- file.path(getwd(),"Dados", "IPC_significado.csv")
con <- file(caminho, "w")
write.table(codigos_ipc_sig, file = con, sep = ";",row.names=FALSE)
close.connection(con)