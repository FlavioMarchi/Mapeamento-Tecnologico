### Países Europa ####
colocar_europa <- function(dados_mapa){
caminho <- file.path(getwd(), "Dados", "paisesEPO.xlsx")
dados <- read_excel(caminho,col_types = "text")

for (i in c(1,2)){
dados[[i]] <- str_trim(dados[[i]])
}
dados[[2]] <- str_to_title(dados[[2]])
dados$continente <- c("European Union")
dados <- left_join(dados, dados_mapa, by = c("continente" = "country"))
juntar <- data_frame(country = dados$pais,
                     total = dados$total)
juntos <- rbind(dados_mapa, juntar)
juntos <- juntos %>% group_by(country) %>% 
  summarize(total = sum(total)) %>% 
  filter(!is.na(total))
return(juntos)
}
