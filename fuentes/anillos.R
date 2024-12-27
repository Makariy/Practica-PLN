source("lib.R")


# Tarda mucho
# SPANISH_WORDS_TO_SYLLABLES <- get_words_syllables(SPANISH_WORDS)
SPANISH_WORDS_TO_SYLLABLES <- readRDS("../resultados/es_words_syllables.rds")
names(SPANISH_WORDS_TO_SYLLABLES) <- SPANISH_WORDS 


# Redefenimos la funcion para mayor velocidad 
get_word_syllables <- function(word) {
  return (SPANISH_WORDS_TO_SYLLABLES[[word]])
}


end_cycle <- function(
  word, 
  final_word
) {
  final_word_syllables <- get_word_syllables(final_word)
  word_syllables <- get_word_syllables(word)
  continuations <- get_words_starting_by_prefix(
    word_syllables[length(word_syllables)],
    SPANISH_WORDS
  )
  ending_words <- get_words_ending_by_postfix(
    final_word_syllables[1], 
    SPANISH_WORDS
  )
  intersection <- intersect(continuations, ending_words)
  if (is.null(intersection)) 
    return (NULL)
  
  for (word in intersection) {
    ending_syllables <- get_word_syllables(word)
    if (
      (ending_syllables[1] == word_syllables[length(word_syllables)])
      &&
      (ending_syllables[length(ending_syllables)] == final_word_syllables[1])
    )
      return (c(word, final_word))
  }
  
  return (NULL)
}

create_cycle_ <- function(
    word, 
    used_words, 
    final_word, 
    iterations_top 
) {
  syllables <- get_word_syllables(word)
  continuations <- get_words_starting_by_prefix(
    syllables[length(syllables)],
    SPANISH_WORDS
  )
  if (length(continuations) == 0)
    return (NULL)
  
  if (iterations_top <= 0) {
    ending <- end_cycle(word, final_word)
    if (!is.null(ending))
      return (ending)
  }
  
  for (continuation in continuations) {
    if (continuation %in% used_words)
      next 
    
    if (get_word_syllables(continuation)[1] != syllables[length(syllables)])
      next 
    
    cycle <- create_cycle_(
      continuation, 
      append(used_words, continuation),
      final_word,
      iterations_top - 1
    )
    if (is.null(cycle)) 
      next 
    
    return (c(continuation, cycle))
  }
}


is_able_to_create_cycle <- function(word) {
  start <- get_word_syllables(word)[1]
  continuations <- get_words_ending_by_postfix(start, SPANISH_WORDS)
  for (word in continuations) {
    syllables <- get_word_syllables(word)
    if (start == syllables[length(syllables)]) 
      return (TRUE)
  }
  return (FALSE)
}

create_cycle <- function(word, top) {
  if (!is_able_to_create_cycle(word)) {
    cat(
      paste(
        "No se va a poder crear un bucle con esta palabra ya que no hay",
        "ninguna otra que termine con la silaba con la que empieza la palabra",
        "pedida",
        sep = " "
      )
    )
    return (NULL)
  }
  return (c(word, create_cycle_(word, c(word), word, top)))
}

result <- create_cycle("loros", 3)






















