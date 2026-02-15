## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(glue)

## -----------------------------------------------------------------------------
glue("glue ", "some ", "text ", "together")

## -----------------------------------------------------------------------------
name <- "glue"
glue("We are learning how to use the {name} R package.")

## -----------------------------------------------------------------------------
release_date <- as.Date("2017-06-13")
glue("Release was on a {format(release_date, '%A')}.")

## -----------------------------------------------------------------------------
glue("
  A formatted string
  Can have multiple lines
    with additional indention preserved
  "
)

## -----------------------------------------------------------------------------
foo <- function() {
  glue("
    A formatted string
    Can have multiple lines
      with additional indention preserved")
}
foo()

## -----------------------------------------------------------------------------
release_date <- as.Date("2017-06-13")
glue("
  The first version of the glue package was released on \\
  a {format(release_date, '%A')}.")

## -----------------------------------------------------------------------------
glue(
  "The first version of the glue package was released on ",
  "a {format(release_date, '%A')}."
)

## -----------------------------------------------------------------------------
# no leading or trailing newline
x <- glue("
  blah
  ")
unclass(x)

# both a leading and trailing newline
y <- glue("

  blah

  ")
unclass(y)

## -----------------------------------------------------------------------------
x <- glue('
  abc
  "	}

  xyz')
class(x)

x
unclass(x)
as.character(x)

## -----------------------------------------------------------------------------
glue("The name of the package is {name}, not {{name}}.")

## -----------------------------------------------------------------------------
fn_def <- "
  <<NAME>> <- function(x) {
    # imagine a function body here
  }"
glue(fn_def, NAME = "my_function", .open = "<<", .close = ">>")

## ----eval = FALSE-------------------------------------------------------------
#  glue(..., .envir = parent.frame())

## -----------------------------------------------------------------------------
x <- "the caller environment"
glue("By default, `glue()` evaluates code in {x}.")

## -----------------------------------------------------------------------------
x <- "the local environment"
glue(
  "`glue()` can access values from {x} or from {y}. {z}",
  y = "named arguments",
  z = "Woo!"
)

## -----------------------------------------------------------------------------
mini_mtcars <- head(cbind(model = rownames(mtcars), mtcars))
rownames(mini_mtcars) <- NULL
glue_data(mini_mtcars, "{model} has {hp} hp.")

## ----eval = getRversion() >= "4.1.0"------------------------------------------
mini_mtcars |>
  glue_data("{model} gets {mpg} miles per gallon.")

## -----------------------------------------------------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
colnames(iris) <- gsub("[.]", "_", tolower(colnames(iris)))
DBI::dbWriteTable(con, "iris", iris)
var <- "sepal_width"
tbl <- "iris"
num <- 2
val <- "setosa"
glue_sql("
  SELECT {`var`}
  FROM {`tbl`}
  WHERE {`tbl`}.sepal_length > {num}
    AND {`tbl`}.species = {val}
  ", .con = con)

## -----------------------------------------------------------------------------
sql <- glue_sql("
  SELECT {`var`}
  FROM {`tbl`}
  WHERE {`tbl`}.sepal_length > ?
", .con = con)
query <- DBI::dbSendQuery(con, sql)
DBI::dbBind(query, list(num))
DBI::dbFetch(query, n = 4)
DBI::dbClearResult(query)

## -----------------------------------------------------------------------------
sub_query <- glue_sql("
  SELECT *
  FROM {`tbl`}
  ", .con = con)

glue_sql("
  SELECT s.{`var`}
  FROM ({sub_query}) AS s
  ", .con = con)

## -----------------------------------------------------------------------------
glue_sql("SELECT * FROM {`tbl`} WHERE sepal_length IN ({vals*})",
  vals = 1, .con = con)

glue_sql("SELECT * FROM {`tbl`} WHERE sepal_length IN ({vals*})",
  vals = 1:5, .con = con)

glue_sql("SELECT * FROM {`tbl`} WHERE species IN ({vals*})",
  vals = "setosa", .con = con)

glue_sql("SELECT * FROM {`tbl`} WHERE species IN ({vals*})",
  vals = c("setosa", "versicolor"), .con = con)

