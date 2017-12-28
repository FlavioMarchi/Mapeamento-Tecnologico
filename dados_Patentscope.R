library(stringi)
library(rebus)
library(tidyverse)
library(ggplot2)

source("grafico_mundo.R")
source("nomear_paises.R")

pattern_1 <- ":ctrName\" class=\"rf-dt-c columnAlign\">"
pattern_2 <- "</td><td id=\"fieldsDefTable:"
#pais
pat_pais <- capture(one_or_more(char_class(WRD,SPC,"(",")",DOT)))





pattern_3 <- "class=\"rf-dt-c columnAlign\"><label style=\"text-align:right\">"
#total
pattern_4 <- "</label></td><td id=\"fieldsDefTable:"

pat_num <- capture(zero_or_more(char_class(DGT,",")))

x <- stri_read_lines("textoPatentscope.txt")

matchesPais <- str_match_all(x, pattern = pattern_1 %R% pat_pais %R% pattern_2)
matchesNum  <- str_match_all(x, pattern = pattern_3 %R% pat_num %R% pattern_4)

pais <- matchesPais[[1]][,2]
npat <- matchesNum[[1]][,2]
npat <- npat[-c(1,3,7,14,17,20,26,34,40,42,48,52,56,58,65,68)]
npat <- npat %>% 
  str_replace_all(pattern = ",", replacement = "") %>% 
  as.numeric() 
(pais <- nomear_paises(pais, flag = FALSE))
grafico_mundo (country = pais, numeros = npat)