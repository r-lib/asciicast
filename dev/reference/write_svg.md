# Create animated SVG from an asciicast

Create animated SVG from an asciicast

## Usage

``` r
write_svg(
  cast,
  path,
  window = NULL,
  start_at = NULL,
  end_at = NULL,
  at = NULL,
  cursor = NULL,
  rows = NULL,
  cols = NULL,
  padding = NULL,
  padding_x = NULL,
  padding_y = NULL,
  omit_last_line = NULL,
  theme = NULL,
  show = NULL
)
```

## Arguments

- cast:

  `asciicast` object.

- path:

  Path to the SVG file to create.

- window:

  Render with window decorations.

- start_at:

  Lower range of timeline to render in seconds.

- end_at:

  Upper range of timeline to render in seconds.

- at:

  Timestamp of single frame to render, in seconds. Alternatively it can
  be `"end"`, to take a snapshot at the end of the cast, after all
  output is done.

- cursor:

  Enable cursor rendering.

- rows:

  Height in lines.

- cols:

  Width in columns.

- padding:

  Distance between text and image bounds.

- padding_x:

  Distance between text and image bounds on x axis.

- padding_y:

  Distance between text and image bounds on y axis.

- omit_last_line:

  Whether to omit the last line of the cast. This often just the prompt,
  and sometimes it is not worth showing.

- theme:

  A named list to override the default theme (see
  [`default_theme()`](https://asciicast.r-lib.org/dev/reference/default_theme.md)).

- show:

  Whether to show the SVG file on the screen, in the viewer pane in
  RStudio, or in the web browser.

## See also

Other SVG functions:
[`default_theme()`](https://asciicast.r-lib.org/dev/reference/default_theme.md),
[`play()`](https://asciicast.r-lib.org/dev/reference/play.md)

## Examples

``` r
cast <- read_cast(system.file("examples", "hello.cast", package = "asciicast"))
svg_file <- tempfile(fileext = ".svg")
write_svg(cast, svg_file)
```
