
<!-- README.md is generated from README.Rmd. Please edit that file -->

# asciicast

> Turn R scripts into terminal screencasts

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Linux Build
Status](https://travis-ci.org/r-lib/asciicast.svg?branch=master)](https://travis-ci.org/r-lib/asciicast)
[![Windows Build
status](https://ci.appveyor.com/api/projects/status/github/r-lib/asciicast?svg=true)](https://ci.appveyor.com/project/gaborcsardi/asciicast)
[![CRAN
status](https://www.r-pkg.org/badges/version/asciicast)](https://cran.r-project.org/package=asciicast)
<!-- badges: end -->

asciicast takes an R script and turns it into an
[asciinema](https://asciinema.org/) cast. It can simulate typing, and
records all terminal output in real time as it happens.

## Features

  - Input is an R script, output is a [v2 asciicast
    recording](https://github.com/asciinema/asciinema/blob/develop/doc/asciicast-v2.md).
  - Record all terminal output in real time, as it happens.
  - Simulate typing in the commands, with a configurable, randomized
    speed.
  - Alternatively, whole comment blocks or expressions can just appear
    on the screen.
  - Convert casts to SVG images using
    [svg-term](https://github.com/marionebl/svg-term). The package comes
    with its own svg-term bundle, no external dependencies are needed.
  - Render a single frame of a cast as an SVG image.
  - Configurable delay at the beginning, at the end and between
    paragraphs.
  - [HTML widget](http://www.htmlwidgets.org), to be used in Rmarkdown
    documents, e.g. in vignettes.
  - Read casts from asciinema JSON files (version 2), or from
    <https://asciinema.org> directly.
  - Special knitr engine to create R markdown files with ascii casts.
    See the `asciicast-demo` vignette.
  - Create ascii casts in GitHub READMEs via animated SVG files. See an
    example in `inst/examples` or the `README.Rmd` source of the README
    file you are reading.

## Limitations

  - asciicast does not work in Windows yet, because of a the lack of a
    pseudo terminal. Maybe you can try running it on Linux in Docker?
  - Recordings are currently real time, so if you “type in” a lot of
    code/text, that might take a while to record.
  - Only syntactically correct R script files can be recorded.
  - asciicast redefines `option("error")` currently, so if you want to
    set this option in your demo, that won’t work.

## Installation

You can install the released version of asciicast from
[CRAN](https://CRAN.R-project.org):

``` r
install.packages("asciicast")
```

## Examples

See the [`inst/examples`
directory](https://github.com/gaborcsardi/asciicast/tree/master/inst/examples)
for these examples.

### Hello world

The input script:

``` 

#' Rows: 10

print("Hello world!")
```

<img src="man/figures/README-/unnamed-chunk-3.svg" width="100%" />

### Asciicast demo in asciicast

Input script that uses asciicast itself:

``` 

#' Title: asciicast example recorded in asciicast
#' Cols: 80
#' Rows: 40
#' Empty_wait: 3
#' End_wait: 20

# <<
# An example for using asciicast, recorded in asciicast itself!

# First, save the R code you want to run, in a script file.
# The file can contain any code, including interactive code,
# as long as it is a syntactically valid R file.

# Second, perform the recording with the `record()` function.
# We are recording an example file now, that comes with the package.
# <<

src <- system.file("examples", "hello.R", package = "asciicast")
cast <- asciicast::record(src)

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
asciicast::write_svg(cast, svg, window = TRUE)
```

<img src="man/figures/README-/unnamed-chunk-5.svg" width="100%" />

### Errors are recorded

Input script with errors:

``` 

#' Rows: 15

# Demonstrate that errors are handled well

library("not-this-really")

traceback()

1+1
```

<img src="man/figures/README-/unnamed-chunk-7.svg" width="100%" />

## Related tools

  - asciinema: <https://asciinema.org/>
  - The original terminal session recorder:
    <https://github.com/asciinema/asciinema>
  - svg-term: <https://github.com/marionebl/svg-term>,
    <https://github.com/marionebl/svg-term-cli>

## License

MIT @ [RStudio](https://github.com/rstudio)
