# Incluimos las funciones de uso 
source("lib.R")



run_game <- function() {
  last_word = NULL 
  last_word_syllables = NULL 
  
  while (TRUE) {
    word <- readline(prompt = "Introduce una palabra: ")
    if (word == "q" || word == "quit") {
      if (!is.null(last_word)) 
        cat(
          paste(
            "Una palabra posible seria:",
            get_next_word(last_word, SPANISH_WORDS)
          )
        )
      break 
    }
    
    # Comprobamos que la longitud es correcta
    if (nchar(word) < 5) {
      cat("La palabra tiene que ser por lo menos de 5 letras\n")
      next
    }
    
    word_syllables <- get_word_syllables(word)
    # Vemos si la palabra esta en el diccionario
    if (!(word %in% SPANISH_WORDS)) {
      cat("Esta palabra no existe.\n")  
      cat("Vuelve a introducir una correcta\n")
      next 
    }
    
    if (!is.null(last_word)) {
      # Vemos si coinciden las silabas 
      if (last_word_syllables[length(last_word_syllables)] != word_syllables[1]) {
        cat(
          paste(
            "Las silabas de la palabra introducida son:", 
            paste(word_syllables, collapse = "-"),
            "\n"
          )
        )
        cat("La ultima silaba de la palabra no coincide con la primera de la introducida\n")
        cat("Vuelve a introducir una correcta\n")
        next 
      }
    }
    
    # La palabra introducida es correcta
    cat(
      paste(
        "La palabra introducida:",
        word,
        paste(word_syllables, collapse = "-"),
        "\n"
      )
    )
    
    # Buscamos la respuesta 
    # O bien buscamos una palabra que no se pueda continuar 
    # answer <- get_next_word_without_continuation(word, SPANISH_WORDS)
    # if (is.null(answer))
    #   answer <- get_next_word(word, SPANISH_WORDS)
    # O jugamos al azar eligiendo cualquier palabra correcta para continuar 
    answer <- get_next_word(word, SPANISH_WORDS)
    
    answer_syllables <- get_word_syllables(answer)
    if (is.null(answer)) {
      cat("No se ha podido encontrar la continuacion.\n")
      cat("Usted ha ganado!\n")
      break 
    }
    cat(
      paste(
        "La respuesta:", 
        answer, 
        paste(answer_syllables, collapse = "-"),
        "\n",
        collapse = " "
      )
    )
    
    # Comprobamos si existe alguna solucion 
    possible_user_input <- get_next_word(answer, SPANISH_WORDS)
    if (is.null(possible_user_input)) {
      cat("Ya no se puede seguir. Ha perdido\n")
      break
    }
    
    last_word <- answer 
    last_word_syllables <- answer_syllables 
  }
}

run_game()
