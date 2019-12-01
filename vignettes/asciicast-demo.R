## ----cache = TRUE, engine="asciicast"-------------------------------------------------------------
# This will be an ascii cast
# Let's evaluate some code
head(mtcars)

## ----cache = TRUE, engine="asciicast"-------------------------------------------------------------
#' Rows: 5

# Not much here, so avoid a big widget
1 + 2 + 3 + 4

## ----code = readLines("../inst/examples/hello.R"), cache = TRUE, engine="asciicast"---------------

#' Rows: 10

print("Hello world!")

## -------------------------------------------------------------------------------------------------
options(asciicast_knitr_svg = TRUE)

## ---- cache = TRUE, engine="asciicast"------------------------------------------------------------
#' Rows: 3
cli::rule(center = "TITLE", line = "~-", line_col = "blue")

## ---- cache = TRUE, fig.with = 15, engine="asciicast"---------------------------------------------
#' Rows: 3
#' Cols: 82
#' At: end
#' Window: FALSE
#' Padding: 10
#' Cursor: FALSE
cli::rule(center = "TITLE", line = "~-", line_col = "blue")

## ---- echo = TRUE, cache = TRUE, engine="asciicast"-----------------------------------------------
#' Rows: 5
#' At: end
# This code will be shown
head(mtcars, 3)

