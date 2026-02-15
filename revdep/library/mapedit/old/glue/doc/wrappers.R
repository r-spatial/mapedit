## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(glue)

## -----------------------------------------------------------------------------
my_glue <- function(..., .envir = parent.frame()) {
  glue(..., .open = "<<", .close = ">>", .envir = .envir)
}

## -----------------------------------------------------------------------------
sw_meta <- list(
  name    = "Name of the character",
  height  = "Height (cm)",
  mass    = "Weight (kg)",
  species = "Name of species",
  films   = "List of films the character appeared in"
)

## -----------------------------------------------------------------------------
my_glue = function(...) {
  glue(..., .open = "<<", .close = ">>", .envir = parent.frame())
}

named_list_to_items <- function(x) {
  my_glue("\\item{<<names(x)>>}{<<x>>}")
}

## -----------------------------------------------------------------------------
named_list_to_items(sw_meta)

## ----error = TRUE-------------------------------------------------------------
my_glue_WRONG <- function(...) {
  glue(..., .open = "<<", .close = ">>")
}

named_list_to_items_WRONG <- function(x) {
  my_glue_WRONG("\\item{<<names(x)>>}{<<x>>}")
}

named_list_to_items_WRONG(sw_meta)

## ----eval = FALSE-------------------------------------------------------------
#  glue(..., .envir = parent.frame(), ...)

## -----------------------------------------------------------------------------
x <- 0
y <- 0
z <- 0

glue("{x} {y} {z}")

## -----------------------------------------------------------------------------
my_glue1 <- function(...) {
  x <- 1
  glue(...)
}

my_glue1("{x} {y} {z}")

## -----------------------------------------------------------------------------
my_glue2 <- function(...) {
  x <- 2
  y <- 2
  my_glue1(...)
}

my_glue2("{x} {y} {z}")

## -----------------------------------------------------------------------------
my_glue3 <- function(..., .envir = parent.frame()) {
  x <- 3
  glue(..., .envir = .envir)
}

my_glue3("{x} {y} {z}")

my_glue4 <- function(...) {
  x <- 4
  y <- 4
  my_glue3(...)
}

my_glue4("{x} {y} {z}")

