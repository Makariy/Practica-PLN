if (!require("sylly")) install.packages("sylly")
if (!require("sylly.es")) install.sylly.lang(c("es"))

library("sylly")
library("sylly.es")
library("stringi")
library("stringr")


WORDS_FILENAME = "../datos/es_words.txt"


# Carga las palabras del fichero y las devuelve como una cadena 
load_words <- function(filename) {
  words_string_lines <- stringr::str_to_lower(readLines(filename))
  words_string <- paste(words_string_lines, collapse = " ")
  words <- unlist(strsplit(words_string, " "))
  return (words[nchar(words) >= 5])
}

# Quita las tildes de las palabras 
remove_accents_from_word <- function(word) {
  removed <- stri_replace_all_regex(
    word,
    pattern = c("[áàäâã]", "[éèëê]", "[íìïî]", "[óòöôõ]", "[úùüû]", "[ÁÀÄÂÃ]", "[ÉÈËÊ]", "[ÍÌÏÎ]", "[ÓÒÖÔÕ]", "[ÚÙÜÛ]"),
    replacement = c("a", "e", "i", "o", "u", "A", "E", "I", "O", "U"),
    vectorize_all = FALSE
  )
  return(removed)
}

# Devuelve las silabas de una palabra 
get_word_syllables <- function(word) {
  word_hyphen <- sylly::hyphen(
    remove_accents_from_word(word), 
    hyph.pattern = "es",
    min.length = 2,
    quiet = TRUE
  )
  syllables <- strsplit(word_hyphen[["word"]], "-")
  return (unlist(syllables))
}

get_words_syllables <- function(words) {
  words <- unlist(lapply(words, remove_accents_from_word))
  words_hyphen <- sylly::hyphen(
    words, 
    hyph.pattern = "es",
    min.length = 2,
    quiet = FALSE
  )
  return (
    strsplit(words_hyphen[["word"]], "-")
  )
}

# Devuelve las palabras que empiezan por <prefix>
get_words_starting_by_prefix <- function(prefix, possible_words) {
  return (possible_words[which(startsWith(possible_words, prefix))])
}

# Devuelve las palabras que terminen con <postfix>
get_words_ending_by_postfix <- function(postfix, possible_words) {
  return (possible_words[which(endsWith(possible_words, postfix))])
}


get_words_starting_by_syllable <- function(syllable, possible_words) {
  candidates <- get_words_starting_by_prefix(syllable, possible_words)
  if (length(candidates) == 0) 
    return (NULL)
  
  words_syllabes <- get_words_syllables(candidates)
  words <- c()
  for (i in 1:length(candidates)) {
    word <- candidates[i]
    syllables <- words_syllabes[[i]]
    if (syllables[1] != syllable)
      next 
    words <- append(words, word)
  }
  return (words)
}

# Devuelve una palabra al azar que empieza por el prefix dado 
get_random_word_by_prefix <- function(prefix, possible_words) {
  words_starting_by_prefix <- get_words_starting_by_prefix(
    prefix, 
    possible_words
  )
  if (length(words_starting_by_prefix) == 0) {
    return (NULL);
  }
  random_index <- sample(1:length(words_starting_by_prefix), 1)
  return (words_starting_by_prefix[random_index])
}


# Devuelve la palabra siguiente para el juego 
get_next_word <- function(word, possible_words) {
  syllables <- get_word_syllables(word)
  last_syllable <- syllables[length(syllables)]
  
  prefix_words <- get_words_starting_by_prefix(
    syllables[length(syllables)], 
    possible_words
  )
  while (length(prefix_words) != 0) {
    result <- get_random_word_by_prefix(
      last_syllable,
      prefix_words
    )
    if (is.null(result))
      break 
    
    result_syllables <- get_word_syllables(result)
    if (result_syllables[1] == last_syllable && result != word)
      return (result)
    
    prefix_words <- prefix_words[prefix_words != result]
  }
  return (NULL)
}

# Devuelve una palabra con la que no se pueda seguir 
get_next_word_without_continuation <- function(word, possible_words) {
  syllables <- get_word_syllables(word)
  last_syllable <- syllables[length(syllables)]
  
  prefix_words <- get_words_starting_by_prefix(
    syllables[length(syllables)], 
    possible_words
  )
  while (length(prefix_words) != 0) {
    result <- get_random_word_by_prefix(
      last_syllable,
      prefix_words
    )
    if (is.null(result))
      break 
    
    result_syllables <- get_word_syllables(result)
    if (result_syllables[1] != last_syllable || result == word)
      next 
    
    next_word <- get_next_word(result, possible_words)
    if (is.null(next_word))
      return (result)
    
    prefix_words <- prefix_words[prefix_words != result]
  }
  return (NULL)
  
} 


# Construye una cadena de palabras 
build_words_chain <- function(word, words) {
  chain <- c(word)
  next_word <- get_next_word(word, words)
  while (!is.null(next_word) && !(next_word %in% chain)) {
    chain <- append(chain, next_word)
    next_word <- get_next_word(next_word, words)
  }
  return (chain)
}

SPANISH_WORDS <- load_words(WORDS_FILENAME)





