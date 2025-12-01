# Create a HTML snapshot of an asciicast

Create a HTML snapshot of an asciicast

## Usage

``` r
write_html(
  cast,
  path,
  at = "end",
  omit_last_line = NULL,
  prefix = "",
  theme = NULL,
  details = FALSE,
  summary = "See output"
)
```

## Arguments

- cast:

  `asciicast` object.

- path:

  Path to the HTML file to create.

- at:

  When to take the snapshot, defaults to the end of the cast (`"end"`).
  Can also be a number, in seconds.

- omit_last_line:

  Whether to omit the last line of the cast. This often just the prompt,
  and sometimes it is not worth showing.

- prefix:

  Prefix to add to the beginning to every line. E.g. `#> ` is usually
  added to knitr output.

- theme:

  A theme name to use, or a a named list to override the default theme
  (see
  [`default_theme()`](https://asciicast.r-lib.org/dev/reference/default_theme.md)).

- details:

  Whether to put the output in a `<details>` tag.

- summary:

  Summary of the `<details>` tag, ignored if `details` is FALSE.
