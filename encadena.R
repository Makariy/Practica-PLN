# Incluir las funciones de uso 
source("lib.R")

word <- "polo"
chain <- build_words_chain(word, SPANISH_WORDS)
print(paste(chain, collapse = " -> "))
