library(tidyverse)
library(readxl)
library(readr)
library(lubridate)
library(stringr)
library(countrycode)
library(RColorBrewer)
library(scales)
rm(list = ls())

# informações sobre a base de dados Espacenet
nome <- "A07.xlsx"
caminho <- file.path(getwd(),nome)
dados <- read_excel(caminho, skip = 6)
dados[is.na(dados)]<- 0
names(dados) <- c("Ano",
                  "China",
                  "EUA",
                  "Japão",
                  "Coreia",
                  "UE"
                  )
dados <- dados %>%
  gather("Pais", "NPatentes", China:UE)
p <- ggplot(dados, aes(x = Ano, y = NPatentes, color = Pais)) +
  geom_line(size = 2)+
  theme_bw()+
  theme(legend.position = "top") +
  theme(legend.title = element_text(colour = "white"))+
  theme(legend.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))+
  theme(axis.text = element_text(size = 12))+
  scale_x_continuous(breaks = c(1883, seq(1890, 2010, 10),2015),
                     limits = c(1880,2016)) +
  ylab(label = "Número de Pedidos") +
  theme(axis.line = element_line(colour = "black", size = 0.5)) +
  scale_y_continuous(breaks = c(0,seq(200000,1200000, 200000)), labels = comma,
                     limits = c(0, 1200000)) +
  theme(panel.grid.major.y = element_line(color = "grey80")) +
  theme(panel.grid.minor = element_line(color = "white"))+
  theme(panel.grid.major.x = element_line(color = "white"))

  # theme(legend.background = element_rect(fill = "white"),
  #       legend.box.background = element_rect(fill = "white"))+

  # theme(panel.background = element_rect(fill = "white"))
  
  
p

# 
# print("Total por lingua")
# dados3$country <- nomear_paises(dados3$code)
# dados_lingua <- dados3 %>%
#   filter(tipo != "U") %>% 
#   group_by(lingua) %>%
#   summarize(total_lingua = sum(total)) %>%
#   arrange(desc(total_lingua)) %>%
#   filter(total_lingua > 100000)
# 
# dados_lingua$lingua <-  factor(dados_lingua$lingua, levels = dados_lingua$lingua)
# 
# 
# #head(dados_lingua)
# # 
# ggplot(dados_lingua, aes(x = lingua, y = total_lingua/1000000, fill = lingua)) +
#   geom_bar(position = "dodge", stat = "identity") +
#   scale_fill_brewer(palette = "Set1", guide = FALSE) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
#   scale_y_continuous(breaks = seq(0, 25, 5)) +
#   xlab(label = "") +
#   ylab(label = "Total (millions)")
# #   
# # 
# # print("Grafico de dados por pais de deposito")
# dados_country <- dados3 %>%
#   filter(tipo != "U") %>% 
#   group_by(country) %>%
#   summarize(total_country = sum(total)) %>%
#   filter((total_country > 1000000 & country != "World Office")) %>%
#   arrange(desc(total_country))
# # 
# dados_country$country <-  factor(dados_country$country, levels = dados_country$country)
# 
# # 
# ggplot(dados_country, aes(x = country, y = total_country/1000000, fill = country)) +
#   geom_bar(position = "dodge", stat = "identity") +
#   scale_fill_brewer(palette = "Spectral", guide = FALSE) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
#   xlab(label = "") +
#   ylab(label = "Total (millions)") +
#   scale_y_continuous(breaks = seq(0, 20, 5))
# # 
# # dados_country <- dados2 %>% 
# #   group_by(country) %>% 
# #   summarize(total_country = sum(total)) %>% 
# #   filter((country != "World Office")) %>% 
# #   arrange(desc(total_country))