library(tidyverse)
library(readxl)
library(readr)
library(lubridate)
library(stringr)
library(countrycode)
library(RColorBrewer)



source("importar_dados.R")
source("limpar_dados.R")
source("nomear_paises.R")

# informações sobre a base de dados Espacenet
nome <- "escritoriosEspacenet.xlsx"
caminho <- file.path(getwd(),nome)

dados <- read_excel(caminho, skip = 0)
dadosEspacenet <- read_excel(caminho, skip = 0)
colnames(dados)[1] <- paste("Country")
remover <- is.na(dados$Country)
dados2 <- dados[!remover,]
dados3 <- dados2[,-c(5,7)]
colnames(dados3) <- c("code", "tipo", "lingua", "first_pub", "last_pub", "total")
dados3$total <- str_replace_all(dados3$total, " ", "")

paises <- distinct(dados3, Country)

nome <- c("countrycodes.xls")
caminho2 <- file.path(getwd(), nome)
codigos <- read_xls(caminho2, skip = 0)
colnames(codigos) <- c("code", "pais")
codigos$code <- str_to_upper(codigos$code)

dados3$total <- as.numeric(dados3$total)

dados <- dados3 %>% 
  group_by(code) %>% 
  summarise(total = sum(total))

custom_match01 <- c(WO = "World Office", EP = "European Union", SU = "Soviet Union",
                    AP = "", EA = "", OA = "",
                    UK = "United Kingdom")

dados2 <- dados3 %>% 
  group_by(code, lingua) %>% 
  summarise(total = sum(total))


dados2$country <- nomear_paises(dados2$code)

dados2 <- dados2 %>% 
  filter(!(country == "")) %>% 
  group_by(country) %>% 
  summarize(total_country = sum(total))

####################### Grafico Mundo ########################
# source("grafico_mundo.R")
# grafico_mundo(dados2$country, dados2$total_country)




######################## fim gráfico mundo ###################

# theTable <- within(theTable, 
#                    Position <- factor(Position, 
#                                       levels=names(sort(table(Position), 
#                                                         decreasing=TRUE))))


print("Total por lingua")
dados3$country <- nomear_paises(dados3$code)
dados_lingua <- dados3 %>%
  filter(tipo != "U") %>% 
  group_by(lingua) %>%
  summarize(total_lingua = sum(total)) %>%
  arrange(desc(total_lingua)) %>%
  filter(total_lingua > 100000)

dados_lingua$lingua <-  factor(dados_lingua$lingua, levels = dados_lingua$lingua)


#head(dados_lingua)
# 
ggplot(dados_lingua, aes(x = lingua, y = total_lingua/1000000, fill = lingua)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Set1", guide = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
  scale_y_continuous(breaks = seq(0, 25, 5)) +
  xlab(label = "") +
  ylab(label = "Total (milhões)")
#   
# 
# print("Grafico de dados por pais de deposito")
dados_country <- dados3 %>%
  filter(tipo != "U") %>% 
  group_by(country) %>%
  summarize(total_country = sum(total)) %>%
  filter((total_country > 1000000 & country != "World Office")) %>%
  arrange(desc(total_country))
# 
dados_country$country <-  factor(dados_country$country, levels = dados_country$country)
dados_country$country <- mapvalues(dados_country$country, 
          from = c("USA", "Japan","China", "European Union", "Germany", 
                   "South Korea", "UK", "Canada", "France", "Australia",
                   "Soviet Union"), 
          to = c("EUA", "Japão", "China", "União Europeia", "Alemanha",
                 "Coreia do Sul", "Reino Unido", "Canadá", "França", "Austrália",
                 "União Soviética")
          )
# 
ggplot(dados_country, aes(x = country, y = total_country/1000000, fill = country)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Spectral", guide = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
  xlab(label = "") +
  ylab(label = "Total (milhões)") +
  scale_y_continuous(breaks = seq(0, 20, 5))
# 
# dados_country <- dados2 %>% 
#   group_by(country) %>% 
#   summarize(total_country = sum(total)) %>% 
#   filter((country != "World Office")) %>% 
#   arrange(desc(total_country))