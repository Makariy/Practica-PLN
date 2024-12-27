source("lib.R")


NUMBER_OF_CHAINS = 10

random_words <- sample(SPANISH_WORDS, NUMBER_OF_CHAINS)


chains <- list()
for (word in random_words) {
  chains <- append(
    chains, 
    list(build_words_chain(word, SPANISH_WORDS))
  )
}


max_chain <- chains[[1]]
for (chain in chains) {
  if (length(chain) > length(max_chain))
    max_chain <- chain 
}

# Con 1000 cadenas se ha podido llegar a 28 palabras en una cadena
# desfortalezcámosles leste tenerles lestrigón gongorizo zonificara rarificarme merineros roscaderos rosqueaban bandearíamos mostrencos costillaje jeringasen sentadlas lastrare regenérale levitases sesgáramos mostradles lesbianas nastuerzo zoquetearé refractaras rasguñaré retinarán ranciedad dadlos
max_chain

# Sin embargo si no nos ponemos a construir las cadenas al azar, sino 
# hacemos una "busqueda en profundiad", llegaremos a resolver el problema 
# de manera mas eficiente


build_words_chain_for_length <- function(chain, target) {
  if (length(chain) > target)
    return (chain) 
  
  last_word <- chain[length(chain)]
  last_word_syllables <- get_word_syllables(last_word)
  possible_words <- get_words_starting_by_prefix(
    last_word_syllables[length(last_word_syllables)], 
    SPANISH_WORDS
  )
  for (word in possible_words) {
    if (get_word_syllables(word)[1] != last_word_syllables[length(last_word_syllables)]) 
      next
    
    return (
      build_words_chain_for_length(
        append(chain, word), 
        target
      )
    )
  }
  return (chain)
}

chain <- build_words_chain_for_length(c("loro"), 200)

# Asi mismo hacemos algo parecido a una busqueda en profundidad
# hasta llegar a la longitud esperada de la cadena
# (para 200 tarda lo suyo, pero si llega ya que encuentra anillos)










