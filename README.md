
<!-- README.md is generated from README.Rmd. Please edit that file -->

# asciicast

> Turn R scripts into terminal screencasts

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/asciicast)](https://cran.r-project.org/package=asciicast)
[![Codecov test
coverage](https://codecov.io/gh/r-lib/asciicast/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/asciicast?branch=main)
[![R-CMD-check](https://github.com/r-lib/asciicast/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/asciicast/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

asciicast takes an R script and turns it into an
[asciinema](https://asciinema.org/) cast. It can simulate typing, and
records all terminal output in real time as it happens.

## Features

-   Input is an R script, output is a [v2 asciicast
    recording](https://github.com/asciinema/asciinema/blob/develop/doc/asciicast-v2.md).
-   Record all terminal output in real time, as it happens.
-   Simulate typing in the commands, with a configurable, randomized
    speed.
-   Alternatively, whole comment blocks or expressions can just appear
    on the screen.
-   Convert casts to SVG images using
    [svg-term](https://github.com/marionebl/svg-term). The package comes
    with its own svg-term bundle, no external dependencies are needed.
-   Render a single frame of a cast as an SVG image.
-   Configurable delay at the beginning, at the end and between
    paragraphs.
-   [HTML widget](http://www.htmlwidgets.org), to be used in Rmarkdown
    documents, e.g. in vignettes.
-   Read casts from asciinema JSON files (version 2), or from
    <https://asciinema.org> directly.
-   Special knitr engine to create R markdown files with ascii casts.
    See the `asciicast-demo` vignette.
-   Create ascii casts in GitHub READMEs via animated SVG files. See an
    example in `inst/examples` or the `README.Rmd` source of the README
    file you are reading.

## Limitations

-   asciicast needs an R build that contains a shared or static R
    library. This is true for most R builds currently.
-   asciicast works best in an UTF-8 locale. It also works well if all
    output is ASCII, but non-ASCII output.
    (<https://github.com/r-lib/asciicast/issues/36>)

## Installation

You can install the released version of asciicast from
[CRAN](https://CRAN.R-project.org):

``` r
install.packages("asciicast")
```

## Examples

See the [`inst/examples`
directory](https://github.com/r-lib/asciicast/tree/main/inst/examples)
for these examples.

### Hello world

The input script:

``` r
print("Hello world!")
```

The result:

<img src="man/figures/README-/unnamed-chunk-3.svg" width="100%" />

### Asciicast demo in asciicast

Input script that uses asciicast itself:

``` r
#' Title: asciicast example recorded in asciicast
#' Empty_wait: 3
#' End_wait: 20

# An example for using asciicast, recorded in asciicast itself!      #!

# First, save the R code you want to run, in a script file.          #!
# The file can contain any code, including interactive code,         #!
# as long as it is a syntactically valid R file.                     #!

# Second, perform the recording with the `record()` function.        #!
# We are recording an example file now, that comes with the package. #!

src <- system.file("examples", "hello.R", package = "asciicast")
cast <- asciicast::record(src)

# `cast` is an `asciicast` object, which has some metadata and the   #!
# recording itself:                                                  #!

cast

# You can write `cast` to a JSON file that can be played by any      #!
# asciinema player. Or you can write it to an SVG file that can      #!
# be embedded into a web page, or a GitHub README.                   #!

svg <- tempfile(fileext = ".svg")
asciicast::write_svg(cast, svg, window = TRUE)
```

<img src="man/figures/README-/unnamed-chunk-5.svg" width="100%" />

### Errors are recorded

Input script with errors:

``` r
#' End_wait: 20
# Demonstrate that errors are handled well

# Base R error
library("not-this-really")
traceback()

# callr errors are saved to `.Last.error`, including a stack trace
library(cli)
callr::r(function() library("another-failure"))
.Last.error
```

<img src="man/figures/README-/unnamed-chunk-7.svg" width="100%" />

## Related tools

-   asciinema: <https://asciinema.org/>
-   The original terminal session recorder:
    <https://github.com/asciinema/asciinema>
-   svg-term: <https://github.com/marionebl/svg-term>,
    <https://github.com/marionebl/svg-term-cli>

## License

MIT @ [RStudio](https://github.com/rstudio)
