salvar_grafico <- function (p, nomearquivo){
  
  dfe <- str_c(nomearquivo, ".png")
  nomearquivo <- file.path(getwd(),"Saida",dfe)
  
  
  png(nomearquivo, width=1280,height=800)
  print(p)
  dev.off()
}