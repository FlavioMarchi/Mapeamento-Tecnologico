grafico_wordcloudIPC <- function (dados, nomearquivo){

word <- dados$word
freq <- dados$freq

word <- str_sub(word, start = 1, end = 21)
nomearquivo <- str_c(nomearquivo, ".png")
nomearquivo <- file.path(getwd(), "Saida", nomearquivo)
png(nomearquivo, width=4,height=5, units='in', res=200)
wordcloud(word, freq,rot.per = 0, 
          scale=c(2,.8), max.words = 50, 
          colors = brewer.pal(7, "Dark2"))
dev.off()
wordcloud(word, freq, scale=c(2,.8),max.words = 50, colors = brewer.pal(5, "Dark2"))
}