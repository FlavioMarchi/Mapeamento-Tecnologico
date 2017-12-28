manipular_titulo <- function(dados){
source("grafico_wordcloud.R")
outras_palavras <- c(Top200Words, "drone","method", "using",
                     "apparatus", "device", "drones",
                     "unmanned", "aerial", "vehicle",
                     "thereof", "having", "aircraft",
                     "system", "systems", "methods", "uav")
# outras_palavras <- c()
title <- str_trim(dados$title)
title <- str_to_lower(title)
title <- str_replace_all(title, pattern = PUNCT, replacement = "")

# frequency <- freq_terms(title, top = 100, at.least = 3, 
#                         stopwords = c(Top200Words, outras_palavras))
# 
# 
# names(frequency)<- c("word", "freq")


### Term Document Matrix
vec<- VectorSource(title)
corpus_d <- VCorpus(vec)


clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"), outras_palavras))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(str_trim))
  
  return(corpus)
}
corpus_d <- clean_corpus(corpus_d)

tdm <- TermDocumentMatrix(corpus_d)

matrix <- as.matrix(tdm)
words <- rowSums(matrix)

words <- sort(words, decreasing = TRUE)

words_freqs <- data_frame(word = names(words), freq = words)
grafico_wordcloud(words_freqs, nomearquivo = "grafico_titulo_wordcloud")



tdm1 <- removeSparseTerms(tdm, sparse = 0.975)
tdm_m <- as.matrix(tdm1)

# Create tdm_df
tdm_df <- as.data.frame(tdm_m)

# Create tweets_dist
dist <- dist(tdm_df)

# Create hc
hc <- hclust(dist)
caminho <- file.path(getwd(), "Saida","grafico_dendograma_titulo.png")
png(caminho, width=1280,height=800)
# Plot the dendrogram
plot(hc)
dev.off()

# word_associate(title, match.string = c("control"),
#                stopwords = c(Top200Words, outras_palavras),
#                network.plot = FALSE, cloud.colors = c("gray85", "darkred"))


# p <- word_associate(dados$title[1:100], match.string = c("Drone", "flying"), 
#                stopwords = c(Top200Words), network.plot = FALSE,
#                cloud.colours = brewer.pal(n = 3, name = "RdBu"))

# sequencia <- order(words_freqs$freq)
# words_freqs$word <- factor(words_freqs$word, 
#                                   levels = make.unique(words_freqs$word[sequencia]))
graf <- data_frame(word = words_freqs$word[1:10],
                   freq = words_freqs$freq[1:10])
sequencia <- order(graf$freq)
graf$word <- factor(graf$word,
                    levels = make.unique(graf$word[sequencia]))
# Grafico 2 com os significados


p1 <- ggplot(graf, aes(x = word, y = freq, fill = freq)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  theme(axis.text.y = element_text(angle = 0, hjust = 1))+
  coord_flip() +
  scale_fill_continuous(guide = FALSE,
                        low = "#FEB24C", high ="#800026") +
  xlab("Palavra")+
  ylab("Número de Ocorrências") +
  theme_bw()+
  theme(text = element_text(size=30))+
  theme(panel.grid.major.y = element_line(color = "grey80")) +
  theme(panel.grid.minor = element_line(color = "white"))+
  theme(panel.grid.major.x = element_line(color = "grey80"))+
  theme(panel.grid.minor.y = element_line(color = "white"))

salvar_grafico(p = p1, nomearquivo = "grafico_frequencia_titulo")

return(list(words_freqs,
            p1)
       )
}
