patentes_por_pais <- function(dados){

  
# Filtrando dados
dados_pais <- dados %>% 
    group_by(country,year) %>% 
    summarize(total = n()) %>% 
    filter(country %in% c("USA", "China", "South Korea", "European Union","Japan")) %>% 
    filter(year > 2000) %>% 
    ungroup() %>% 
    complete(year, country,fill = list(total = 0))

dados_pais_ano <- dados %>% 
  group_by(country,year) %>% 
  summarize(total = n()) %>%
  filter(year > 2000) %>% 
  arrange(country, year) %>% 
  # ungroup() %>% 
  filter(total != 0) %>% 
  filter(country != "NA") %>% 
  complete(year, country,fill = list(total = 0))

dados_pais02 <- dados %>% 
  group_by(country) %>% 
  summarize(total = n()) %>%
  ungroup() %>% 
  filter(country != "NA") %>% 
  arrange(desc(total))
  
maxyear <- max(dados_pais$year)
minyear <- min(dados_pais$year)

p1 <-ggplot(dados_pais, aes(x = factor(year), y = total, fill = country)) +
    geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
    scale_fill_brewer(palette = "Set1", name = "País") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20)) +
  # ggtitle("Patentes ao Longo dos Anos, por Países")+
  xlab("Ano") +
  ylab("N Patentes")+
  theme(text = element_text(size=30))+
  theme(axis.text.y = element_text(size = 20))+
  theme(legend.text = element_text(color = "black", size = 20))+
  theme(panel.grid.major.y = element_line(color = "grey90")) +
  theme(panel.grid.minor = element_line(color = "white"))+
  theme(panel.grid.major.x = element_line(color = "grey80"))

  #### line plot ######
  
p2 <-ggplot(dados_pais, aes(x = year, y = total, color = country)) +
    geom_line(size = 2) +
  theme_bw()+
  
    scale_color_brewer(palette = "Set1", name = "País") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20)) +
    # ggtitle("Patentes ao Longo dos Anos, por Países") +
    xlab("Ano") +
    ylab("N Patentes") +
  theme(legend.text = element_text(color = "black", size = 20))+
    scale_x_continuous(breaks = seq(minyear, maxyear, 1)) +
  theme(text = element_text(size=30)) +
  theme(axis.text.y = element_text(size = 20))+
  theme(panel.grid.major.y = element_line(color = "grey90")) +
  theme(panel.grid.minor.y = element_line(color = "grey90", linetype = "dashed"))+
  theme(panel.grid.major.x = element_line(color = "grey90")) +
  theme(panel.grid.minor.x = element_line(color = "grey90", linetype = "dashed"))
    # scale_y_continuous(breaks = seq(0, 250, 50))

sequencia <- order(dados_pais02$total, decreasing = TRUE)
pais <- factor(dados_pais02$country, levels = dados_pais02$country[sequencia])
total <- dados_pais02$total
dadosgrafico <- data_frame(pais = pais[1:7],
                           total = total[1:7])

p3 <-ggplot(dadosgrafico, aes(x = pais, y = total, fill = pais)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw() +
  scale_fill_brewer(palette = "Set1", guide = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 25)) +
  # ggtitle("Total de Patentes por País")+
  xlab("País") +
  ylab("N Patentes")+
  theme(text = element_text(size=30)) +
  theme(panel.grid.major.y = element_line(color = "grey90")) +
  theme(panel.grid.minor = element_line(color = "white"))+
  theme(panel.grid.major.x = element_line(color = "grey80"))
  
dados_ano <- dados %>% 
  group_by(year) %>% 
  summarize(total = n())

p4 <-ggplot(dados_ano, aes(x = year, y = total, fill = total, label = total)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(y = total + 20), position = position_stack(vjust = 1)) +
  theme_bw() +
  scale_fill_gradient(guide = FALSE, low = "lightpink", high = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20)) +
  scale_x_continuous(breaks = seq(minyear, maxyear, 1))+
  # ggtitle("Total de Patentes por País")+
  xlab("Ano") +
  ylab("N Patentes")+
  theme(text = element_text(size=30)) +
  theme(panel.grid.major.y = element_line(color = "grey90")) +
  theme(panel.grid.minor = element_line(color = "white"))+
  theme(panel.grid.major.x = element_line(color = "white"))

salvar_grafico(p1, "grafico01")
salvar_grafico(p2, "grafico02")
return(list(p1, 
            p2, 
            p3,
            p4,
            dadosgrafico,
            dados_pais_ano,
            dados_pais02,
            dados_ano))
}
  