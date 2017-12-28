grafico_wordcloud <- function (dados, nomearquivo){

word <- dados$word
freq <- dados$freq

word <- str_sub(word, start = 1, end = 21)
nomearquivo <- str_c(nomearquivo, ".png")
nomearquivo <- file.path(getwd(), "Saida", nomearquivo)
if(file.exists(nomearquivo)){file.remove(nomearquivo)}
png(nomearquivo, width=6,height=4, units='in', res=500)
wordcloud(word, freq,
          scale=c(3,.5), 
          max.words=100, 
          colors =brewer.pal(n = 8, name = "Dark2")
          )
dev.off()
# wordcloud(word, freq, scale=c(2,.8),max.words = 50, colors = brewer.pal(5, "Dark2"))

# wordcloud(word, freq,
#           scale=c(4,.8), 
#           max.words = 100, 
#           colors =rev(brewer.pal(n = 8, name = "Dark2")))


}

# png("wordcloud.png", width=1280,height=800)
# wordcloud(words_freqs$word, words_freqs$freq,
#           max.words = 100, colors = rev(brewer.pal(n = 8, name = "Dark2")),
#           scale=c(8,.3),min.freq=2, random.order=T,
#           rot.per=.15
# )
# dev.off()

