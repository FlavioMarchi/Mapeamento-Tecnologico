patentes_por_applicant <- function(dados){

# reformatar applicant names

app <- separate_rows(dados, applicant, sep = ";")

# Editamos novamente
app$applicant <- str_replace_all(app$applicant, pattern = PUNCT, replacement = "")
app$applicant <- str_trim(app$applicant)
app$applicant <- str_to_title(app$applicant)

app <- app  %>% 
  group_by(applicant) %>% 
  filter (!is.na(applicant)) %>% 
  summarize(total = n()) %>% 
  arrange(desc(total))

applicants <- str_replace_all(app$applicant, pattern = or(">","<"), "")

graf <- data_frame(word = applicants[1:20],
                   freq = app$total[1:20])
sequencia <- order(graf$freq)
graf$word <- factor(graf$word,
                    levels = make.unique(graf$word[sequencia]))
# # Grafico 2 com os significados


p2 <- ggplot(graf, aes(x = word, y = freq, fill = freq)) +
  geom_col(position = "dodge") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  theme(axis.text.y = element_text(angle = 0, hjust = 1))+
  coord_flip() +
  scale_fill_gradient(guide = FALSE, 
                      low ="#e5f5e0",
                        high ="#31a354") +
  xlab("Depositante")+
  ylab("Número de Ocorrências") +
  theme(text = element_text(size=30))+
  theme(panel.grid.major.y = element_line(color = "grey80")) +
  theme(panel.grid.minor = element_line(color = "white"))+
  theme(panel.grid.major.x = element_line(color = "grey80"))+
  theme(panel.grid.minor.y = element_line(color = "white"))
  

salvar_grafico(p = p2, nomearquivo = "grafico_applicants")


return(list(p2,app))
}
