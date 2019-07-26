
#' Title: rsciinema example recorded in rsciinema
#' Cols: 80
#' Rows: 40
#' Empty_wait: 3
#' End_wait: 20

# <<
# An example for using rsciinema, recorded in rsciinema itself!

# First, save the R code you want to run, in a script file.
# The file can contain any code, including interactive code,
# as long as it is a syntactically valid R file.

# Second, perform the recording with the `record()` function.
# We are recording an example file now, that comes with the package.
# <<

src <- system.file("examples", "hello.R", package = "rsciinema")
cast <- rsciinema::record(src)

# <<
# `cast` is an `asciicast` object, which has some metadata and the
# recording itself:
# <<

cast

# <<
# You can write `cast` to a JSON file that can be played by any
# asciinema player. Or you can write it to an SVG file that can
# be embedded into a web page, or a GitHub README.
# <<

svg <- tempfile(fileext = ".svg")
rsciinema::write_svg(cast, svg, window = TRUE)
