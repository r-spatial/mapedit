## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(stringr)

## -----------------------------------------------------------------------------
# English
str_to_upper("i")
str_to_lower("I")

# Turkish
str_to_upper("i", locale = "tr")
str_to_lower("I", locale = "tr")

## -----------------------------------------------------------------------------
dutch_sentence <- "ijsland is een prachtig land in Noord-Europa."

# Incorrect
str_to_sentence(dutch_sentence)
# Correct
str_to_sentence(dutch_sentence, locale = "nl")

## -----------------------------------------------------------------------------
turkish_names <- c("İpek", "Işık", "İbrahim")
search_name <- "ipek"

# incorrect
str_equal(turkish_names, search_name, ignore_case = TRUE)

# correct
str_equal(turkish_names, search_name, ignore_case = TRUE, locale = "tr")

## -----------------------------------------------------------------------------
# incorrect
str_detect(turkish_names, fixed(search_name, ignore_case = TRUE))

# correct
str_detect(turkish_names, coll(search_name, ignore_case = TRUE, locale = "tr"))

## -----------------------------------------------------------------------------
czech_words <- c("had", "chata", "hrad", "chůze")
lithuanian_words <- c("ąžuolas", "ėglė", "šuo", "yra", "žuvis")

# incorrect
str_sort(czech_words)
str_sort(lithuanian_words)

# correct
str_sort(czech_words, locale = "cs")
str_sort(lithuanian_words, locale = "lt")

