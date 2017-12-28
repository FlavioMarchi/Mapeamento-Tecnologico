closeAllConnections()
rm(list=ls())

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
library(GGally)
library(extrafont)



source("importar_dados.R")
source("limpar_dados.R")
source("nomear_paises.R")
source("exportarExcel.R")
source("extrair_ipc_linha.R")
source("extrair_ipc_documento.R")
source("grafico_mundo.R")
source("manipular_ipc.R")
source("patentes_por_pais.R")
source("grafico_wordcloud.R")
source("grafico_wordcloudIPC.R")
source("manipular_titulo.R")
source("salvar_grafico.R")
source("colocar_europa.R")
source("patentes_por_applicant.R")
source("salvar_dados.R")
source("juntar_espacenetXLS.R")
source("juntar_espacenetCSV.R")
source("juntar_patentscope.R")
# dadose <- importar_dados(base = "Espacenet", tipo_arquivo = "xls")
# dadose <- importar_dados(base = "Espacenet", tipo_arquivo = "csv")
dadosp <- importar_dados(base = "Patentscope")

# exportarExcel(dadose, nome_arquivo = "baseEspacenet_limpa")
# exportarExcel(dadosp, nome_arquivo = "basePatentscope_limpa")


##############################################
#################    IPC     #################
##############################################


# xe <- manipular_ipc(dadose)
# xe[[3]] <- xe[[3]] +ggtitle(label = "Espacenet")
# xe[[4]] <- xe[[4]] +ggtitle(label = "Espacenet")
# salvar_grafico(xe[[3]], nomearquivo = "ocorrenciasEspacenet01")
# salvar_grafico(xe[[4]], nomearquivo = "ocorrenciasEspacenet02")
# salvar_dados(xe[[1]], nomearquivo = "freqcodigoIPCespacenet")
# salvar_dados(xe[[2]], nomearquivo = "freqgrupoIPCespacenet")

xp <- manipular_ipc(dadosp)
xp[[3]] <- xp[[3]] +ggtitle(label = "Patentscope")
xp[[4]] <- xp[[4]] +ggtitle(label = "Patentscope")
salvar_grafico(xp[[3]], nomearquivo = "ocorrenciasPatentscope01")
salvar_grafico(xp[[4]], nomearquivo = "ocorrenciasPatentscope02")
salvar_dados(xp[[1]], nomearquivo = "freqcodigoIPCpatentscope")
salvar_dados(xp[[2]], nomearquivo = "freqgrupoIPCpatentscope")



##############################################
#################    T?tulo  #################
##############################################

# xxe <- manipular_titulo(dadose)
# xxe[[2]] <- xxe[[2]] +ggtitle(label = "Espacenet")
# salvar_grafico(xxe[[2]], nomearquivo = "graficoFreqEspacenet")
# salvar_dados(xxe[[1]], nomearquivo = "freqwordEspacenet")

# xxp <- manipular_titulo(dadosp)
# xxp[[2]] <- xxp[[2]] +ggtitle(label = "Patentscope")
# salvar_grafico(xxp[[2]], nomearquivo = "graficoFreqPatentscope")
# salvar_dados(xxp[[1]], nomearquivo = "freqwordPatentscope")


##############################################
################# Mapa Mundo #################
##############################################


# dados_mapae <- dadose %>%
#   group_by(country) %>%
#   summarize(total = n()) %>%
#   ungroup()
# dados_me <- colocar_europa(dados_mapae)
# p_mapae <- grafico_mundo (dados_me)+ggtitle("Espacenet")
# salvar_grafico(p = p_mapae, "graficomapaEspacenet")

dados_mapap <- dadosp %>%
  group_by(country) %>%
  summarize(total = n()) %>%
  ungroup()
dados_mp <- colocar_europa(dados_mapap)
p_mapap <- grafico_mundo (dados_mp) +ggtitle("Patentscope")
salvar_grafico(p = p_mapap, "graficomapaPatentscope")




##############################################
################# Patentes x Pais ############
##############################################

# pe <- patentes_por_pais(dadose)
# salvar_grafico(pe[[1]]+ggtitle("Espacenet"), nomearquivo = "graficoBarraPaisAnoEspacenet")
# salvar_grafico(pe[[2]]+ggtitle("Espacenet"), nomearquivo = "graficoLinhaPaisAnoEspacenet")
# salvar_grafico(pe[[3]]+ggtitle("Espacenet"), nomearquivo = "graficoBarraPaisTotalEspacenet")
# salvar_grafico(pe[[4]]+ggtitle("Espacenet"), nomearquivo = "graficoBarraAnoTotalEspacenet")
# salvar_dados(pe[[7]], nomearquivo = "paisTotalEspacenet")
# salvar_dados(pe[[6]], nomearquivo = "paisAnoTotalEspacenet")
# salvar_dados(pe[[8]], nomearquivo = "AnoTotalEspacenet")

pp <- patentes_por_pais(dadosp)
salvar_grafico(pp[[1]]+ggtitle("Patentscope"), nomearquivo = "graficoBarraPaisAnoPatentscope")
salvar_grafico(pp[[2]]+ggtitle("Patentscope"), nomearquivo = "graficoLinhaPaisAnoPatentscope")
salvar_grafico(pp[[3]]+ggtitle("Patentscope"), nomearquivo = "graficoBarraPaisTotalPatentscope")
salvar_grafico(pp[[4]]+ggtitle("Patentscope"), nomearquivo = "graficoBarraAnoTotalPatentscope")
salvar_dados(pp[[7]], nomearquivo = "paisTotalPatentscope")
salvar_dados(pp[[6]], nomearquivo = "paisAnoTotalPatentscope")
salvar_dados(pp[[8]], nomearquivo = "AnoTotalPatentscope")


###################################################
################# Patentes x Assignees ############
###################################################

# ppe <- patentes_por_applicant(dadose)
# ppe[[1]] <- ppe[[1]]  +ggtitle("Espacenet")
# salvar_dados(ppe[[2]], nomearquivo = "applicantsEspacenet")
# salvar_grafico(ppe[[1]], nomearquivo = "graficoApplicantsEspacenet")

ppp <- patentes_por_applicant(dadosp)
ppp[[1]] <- ppp[[1]]  +ggtitle("Patentscope")
salvar_dados(ppp[[2]], nomearquivo = "applicantsPatentscope")
salvar_grafico(ppp[[1]], nomearquivo = "graficoApplicantsPatentscope")

