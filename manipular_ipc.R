manipular_ipc <- function(dados){

library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(countrycode)
library(RColorBrewer)
library(rebus)
library(XLConnect)
library(scales)
library(qdap)
library(tm)
library(wordcloud)


x <- dados$ipc
x <- str_replace_all(x, pattern = ";", replacement = " ")
# x <- str_replace_all(x, pattern = "/", replacement = "")
vec_source <- VectorSource(x)
v_corpus <- VCorpus(vec_source)
tdm <- TermDocumentMatrix(v_corpus)

matriz <- as.matrix(tdm)

# # Matriz de palavras para WordCloud
# freqWordCloud <- sort(rowSums(matriz), decreasing = TRUE)[1:20]
# word_freqWordCloud <- data_frame(
#   term = names(freqWordCloud),
#   num = freqWordCloud
# )


freq <- sort(rowSums(matriz), decreasing = TRUE)[1:13]
word_freq <- data_frame(
  term = names(freq),
  freq = freq
)

word_freq$term <- str_to_upper(word_freq$term)
word_freq$term <- str_trim(word_freq$term)


#ordenando o vetor por ordem decrescente, e convertendo para factor para conseguir fazer o gráfico corretamente
sequencia <- order(freq)
word_freq$term <- factor(word_freq$term, levels = word_freq$term[sequencia])
word_freq_grafico2 <- word_freq


# wordcloud(word_freq$term, word_freq$num, max.words = 100, colors = "red")
graf01 <- data_frame(term = word_freq_grafico2$term[1:10],
                   freq = word_freq_grafico2$freq[1:10])
p1 <- ggplot(graf01, aes(x = term, y = freq, fill = freq)) +
  geom_col(position = "dodge") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  theme(axis.text.y = element_text(angle = 0, hjust = 1))+
  coord_flip() +
  scale_fill_continuous(guide = FALSE,
                        low ="#deebf7" ,
                        high ="#084594"
                        ) +
  xlab("Código IPC")+
  ylab("Número de Ocorrências") +
  theme(text = element_text(size=30))+
  theme(panel.grid.major.y = element_line(color = "grey80")) +
  theme(panel.grid.minor = element_line(color = "white"))+
  theme(panel.grid.major.x = element_line(color = "grey80"))+
  theme(panel.grid.minor.y = element_line(color = "white"))
  

# Passando o word_freq_grafico2 para o padrão do documento de ipcs
# word_freq_grafico2$term <- str_sub(word_freq_grafico2$term, start = 1, end = 6)
# É preciso passar G08G5/ para G08G05
antes_pattern <- word_freq_grafico2$term

# pattern01 <- capture(WRD%R%DGT%R%DGT%R%WRD%R%repeated(DGT, lo = 1, hi = 3))%R%"/"
# pattern02 <- capture(repeated(DGT, lo = 1, hi = 3))%R%"/"
pattern01 <- START %R% WRD %R% repeated(DGT, lo = 2, hi = 2) %R% WRD
pattern02 <- repeated(DGT, lo = 1, hi = 3) %R% END
pattern00 <- START %R% WRD %R% repeated(DGT, lo = 2, hi = 2) %R% 
              WRD %R% repeated(DGT, lo = 1, hi = 3)
d <- as.character(word_freq_grafico2$term)
dd <- str_extract(d, pattern00)
ddd <- str_extract(dd, pattern01)
num <- str_extract(dd, pattern02)
num <- str_pad(num, width = 4, pad = "0")
codigo <- str_c(ddd, num)

word_freq_grafico2$term <- codigo
word_freq_grafico2 <- word_freq_grafico2 %>% 
                        group_by(term) %>% 
                        summarize(freq = sum(freq))

# Função para "traduzir" B64C39 para "significado"
caminho <- file.path(getwd(),"Dados", "IPC_significado.csv")
con <- file(caminho, "r")
codigos_ipc_sig <- read_csv2(file = caminho, col_types = "cc")
close.connection(con)
codigos_ipc_sig <- data_frame( IPC = codigos_ipc_sig[[1]],
                               significado = codigos_ipc_sig[[2]])
word_freq_grafico2 <- left_join(word_freq_grafico2, codigos_ipc_sig, by = c("term" = "IPC"))

names(word_freq_grafico2) <- c("IPC", "freq", "term")

# word_freq_grafico2[[3]][7] <- str_c(word_freq_grafico2[[3]][7], " ")
# word_freq_grafico2[[3]][8] <- str_replace(word_freq_grafico2[[3]][8], pattern = ",", replacement = "")

# Ordenando termos

sequencia <- order(word_freq_grafico2$freq)
word_freq_grafico2$term <- factor(word_freq_grafico2$term, 
                                  levels = make.unique(word_freq_grafico2$term[sequencia]))

# Grafico 2 com os significados
graf <- data_frame(term = word_freq_grafico2$term,
                   freq = word_freq_grafico2$freq)
p2 <- ggplot(graf, aes(x = term, y = freq, fill = freq)) +
  geom_col(position = "dodge") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  theme(axis.text.y = element_text(angle = 0, hjust = 1))+
  coord_flip() +
  scale_fill_continuous(guide = FALSE,
                        low ="#deebf7" ,
                        high ="#084594") +
  xlab("IPC")+
  ylab("Número de Ocorrências") +
  theme(text = element_text(size=30))+
  theme(panel.grid.major.y = element_line(color = "grey80")) +
  theme(panel.grid.minor = element_line(color = "white"))+
  theme(panel.grid.major.x = element_line(color = "grey80"))+
  theme(panel.grid.minor.y = element_line(color = "white"))

names(word_freq) <- c("word","freq")
names(word_freq_grafico2) <- c("IPC", "freq", "word")

grafico_wordcloudIPC(word_freq, nomearquivo = "grafico_wordcloudIPC01")
grafico_wordcloudIPC(word_freq_grafico2, nomearquivo = "grafico_wordcloudIPC02")
salvar_grafico (p1, "grafico_IPCocorrencias01")
salvar_grafico (p2, "grafico_IPCocorrencias02")

word_freq_grafico2 <- arrange(word_freq_grafico2, desc(freq))

return(list(word_freq,
            word_freq_grafico2, 
            p1,
            p2
            )
       )
}