source("lib.R")

# Una posible opcion seria coger un subconjunto de palabras que 
# empiezen con el mismo prefijo y buscar otras palabras con el mismo
# prefijo que el sufijo de la ultima palabra

# Entonces se podra tener un "diccionario" cuyas "llaves" sean 
# dos silabas <x> e <y> en formato x_y donde <x> es la primera silaba
# e <y> es la ultima. 

# Una vez hemos construido este diccionario, podemos aplicar un 
# algoritmo como el usado en "supercadena" para construir una cadena 
# que empieze y termine con la misma palabra y cada siguiente empieze
# con la silaba de la anterior 

# Sin embargo, esta solucion (como cualquier otra probablemente)
# funciona muy lento a causa de que la libreria sylly tarda mucho 
# descomponiendo la palabra en silabas. Entonces para que el algoritmo
# sea eficiente, vamos primero a descomponer todas las palabras en 
# silabas y ya con ellas ejecutar el algoritmo 


# Tarda mucho
# SPANISH_WORDS_TO_SYLLABLES <- get_words_syllables(SPANISH_WORDS)
SPANISH_WORDS_TO_SYLLABLES <- readRDS("es_words_syllables.rds")
names(SPANISH_WORDS_TO_SYLLABLES) <- SPANISH_WORDS 



# Se podria construir una matriz cuadrada donde filas y columnas 
# correspondan a una palabra y tenga 0 si no se puede continuar y 
# 1 si si se puede, es decir, 
# /////   mundo   domino
# mundo   0       1 
# domino  0       0 
# De esta manera, podriamos empezar con una palabra e intentar
# avanzar lo maximo posible hasta llegar a una palabra con la ultima 
# silaba igual a la primera silaba de la primera palabra 

# Sin embargo, en vez de construir esta matriz directamente, la vamos 
# desarrollar dinamicamente cuando necesitemos 

# Ponemos un limite, por ejemplo 1000 y vamos construyendo la cadena 
# de manrea recursiva implementando "la busqueda en profundidad"
# hasta llegar al limite establecido y despues intentando llegar 
# a la primera palabra 


LETTER_TO_WORDS <- list()
count <- length(SPANISH_WORDS)
for (i in 1:count) {
  word <- SPANISH_WORDS[i]
  if (i %% 1000 == 0) 
    cat("Processing ", i/count, "%\n")
  letter <- substr(word, 1, 1)
  if (letter %in% names(LETTER_TO_WORDS))
    LETTER_TO_WORDS[[letter]] <- append(LETTER_TO_WORDS[[letter]], word)
  else {
    LETTER_TO_WORDS[[letter]] <- list(word)
  }
}


get_words_starting_by_prefix <- function(prefix) {
  letter <- substr(prefix, 1, 1)
  words <- LETTER_TO_WORDS[[letter]]
  return (words[which(startsWith(words, prefix))])
}


get_words_starting_by_syllable <- function(syllable) {
  candidates <- get_words_starting_by_prefix(syllable, SPANISH_WORDS)
  words <- c()
  for (word in candidates) {
    if (SPANISH_WORDS_TO_SYLLABLES[[word]][1] == syllable)
      words <- append(words, word)
  }
  return (words)
}


create_cycle_ <- function(
    last_word, 
    used_words, 
    final_word, 
    iterations_top 
) {
  cat(paste("Iterating for:", last_word, "\n"))
  syllables <- SPANISH_WORDS_TO_SYLLABLES[[last_word]]
  continuations <- get_words_starting_by_syllable(
    syllables[length(syllables)]
  )
  if (length(continuations) == 0)
    return (NULL)
  
  if (iterations_top <= 0 && final_word %in% continuations) {
    return (final_word)
  }
  
  max_cycle <- NULL
  for (word in continuations) {
    if (word %in% used_words)
      next 
    
    cycle <- create_cycle_(
      word, 
      append(used_words, word),
      final_word,
      iterations_top - 1
    )
    if (is.null(cycle)) 
      next 
    
    if (is.null(max_cycle) || length(max_cycle) < length(cycle)) 
     max_cycle <- cycle
  }
  
  return (max_cycle)
}

create_cycle <- function(word, top) {
  return (create_cycle_(word, c(word), word, top))
}

result <- create_cycle("mundo", 10)






















