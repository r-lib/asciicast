# The default asciicast theme

Currently only used for
[`write_svg()`](https://asciicast.r-lib.org/dev/reference/write_svg.md)

## Usage

``` r
default_theme()
```

## Value

A named list.

## See also

Other SVG functions:
[`play()`](https://asciicast.r-lib.org/dev/reference/play.md),
[`write_svg()`](https://asciicast.r-lib.org/dev/reference/write_svg.md)

## Examples

``` r
cast <- read_cast(system.file("examples", "hello.cast", package = "asciicast"))
svg_file <- tempfile(fileext = ".svg")
mytheme <- modifyList(default_theme(), list(cursor = c(255, 0, 0)))
write_svg(cast, svg_file, theme = mytheme)
```
