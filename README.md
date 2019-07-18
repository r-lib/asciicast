
<!-- README.md is generated from README.Rmd. Please edit that file -->

# asciicast

> Turn R scripts into R ascii screencasts

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/asciicast)](https://cran.r-project.org/package=asciicast)
[![Linux Build
Status](https://travis-ci.org/r-lib/asciicast.svg?branch=master)](https://travis-ci.org/r-lib/asciicast)
<!-- badges: end -->

asciicast takes an R script and turns it into an
[asciiname](https://asciinema.org/) cast. It can simulate typing, and
records all terminal output in real time as it happens.

## Features

  - Input is a simple R script, output is a [v2 asciinema
    recording](https://github.com/asciinema/asciinema/blob/develop/doc/asciicast-v2.md).
  - Record all terminal output in real time, as it happens.
  - Alternatively, whole comment blocks or expressions can just appear
    on the screen.
  - Convert casts to SVG images using
    [svg-term](https://github.com/marionebl/svg-term). The package comes
    with its own svg-term bundle, no external dependencies are needed.
  - Render a single frame of a cast as an SVG image.
  - Configurable delay at the beginning, at the end and between
    paragraphs.

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

### Hello world

TODO

### Errors are recorded

TODO

### 

## Related tools

  - asciinema: <https://asciinema.org/>
  - The original terminal session recorder:
    <https://github.com/asciinema/asciinema>
  - svg-term: <https://github.com/marionebl/svg-term>,
    <https://github.com/marionebl/svg-term-cli>

## License

MIT @ [RStudio](https://github.com/rstudio)
