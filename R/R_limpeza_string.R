# Remover caracter e especial e palavras de ligação de string

palavrasLigacao <- function(variavel){
# Remover pontuações
  variavel <- variavel |>
    stringr::str_to_lower() |>
    stringr::str_remove_all("[[:punct:]]|") |>
    stringr::str_remove_all("[\t\n\r\f\v]")
# Palavras de ligação
  remover <-read.csv("https://gist.githubusercontent.com/alopes/5358189/raw/2107d809cca6b83ce3d8e04dbd9463283025284f/stopwords.txt",header = FALSE)
  remover$V1 <- tolower(gsub("[[:space:]]","",as.character(remover$V1)))
  for(i in 1:nrow(remover))
    {variavel <- gsub(paste0(" ",remover[i,1]," "), " ", variavel)}

  variavel <- ifelse(stringr::str_sub(variavel,1,2) %in% c("o ", "a "),
                     stringr::str_sub(variavel,3),variavel)
  variavel <- abjutils::rm_accent(variavel)
  variavel <- stringr::str_trim(gsub("NA","",variavel))
  # rm(remover)
  variavel
}

#x = c("Teste !?", "Tem numero","Caracter 1~ é ´1"," de onde veio, para onde vai","o cara é você")

#palavrasLigacao(x)