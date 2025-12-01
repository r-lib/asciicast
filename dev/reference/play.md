# Play asciinema cast as an SVG image in the default browser

Uses
[`write_svg()`](https://asciicast.r-lib.org/dev/reference/write_svg.md)
to create an SVG image for a cast, in a temporary file, and then
previews a minimal HTML file with the SVG image, in the default browser.

## Usage

``` r
play(cast, ...)
```

## Arguments

- cast:

  `asciicast` object

- ...:

  Additional arguments are passed to
  [`write_svg()`](https://asciicast.r-lib.org/dev/reference/write_svg.md).

## Value

The path of the temporary SVG file, invisibly.

## See also

Other SVG functions:
[`default_theme()`](https://asciicast.r-lib.org/dev/reference/default_theme.md),
[`write_svg()`](https://asciicast.r-lib.org/dev/reference/write_svg.md)

## Examples

``` r
if (FALSE) { # interactive()
cast <- read_cast(system.file("examples", "hello.cast", package = "asciicast"))
play(cast)
}
```
