exportarExcel <- function(dados, nome_arquivo){
  library(XLConnect)
  dfe <- str_c(nome_arquivo, ".xlsx")
  nome_excel <- file.path(getwd(),"Saida",dfe)
  # Verificando se o arquivo já existe, e se existir, apagar
  if(file.exists(nome_excel)){
    x <- file.remove(nome_excel)
  }
  
  wb <- loadWorkbook(nome_excel, create = TRUE)
  
  #creating sheets within an Excel workbook
  createSheet(wb, name = "dados")
  
  #writing into sheets within an Excel workbook : 
  #writing ChickWeight data frame into chickSheet
  writeWorksheet(wb, dados, sheet = "dados", rownames = TRUE)
  
  #saving a workbook to an Excel file :
  #saves a workbook to the corresponding Excel file and writes the file to disk.
  saveWorkbook(wb, file = nome_excel)
}