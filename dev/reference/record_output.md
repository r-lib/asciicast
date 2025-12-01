# Record output of an R script and return it as a character vector

This function uses
[`record()`](https://asciicast.r-lib.org/dev/reference/record.md)
internally, but instead of creating an ascii cast, it just returns the
output of the code in a character vector.

## Usage

``` r
record_output(
  script,
  echo = FALSE,
  prompt = echo,
  stdout = TRUE,
  stderr = TRUE,
  ...
)
```

## Arguments

- script:

  The code to record, passed to
  [`record()`](https://asciicast.r-lib.org/dev/reference/record.md).

- echo:

  Whether to include the input in the return value.

- prompt:

  Whether to include the R prompt in the return value.

- stdout:

  Whether to include the standard output in the return value.

- stderr:

  Whether to include the standard error in the return value.

- ...:

  Addiitonal arguments are passed to
  [`record()`](https://asciicast.r-lib.org/dev/reference/record.md).
  (You cannot use `typing_speed` and `echo`, though, because these are
  used internally by `record_output()`.

## Value

Character vector of output (plus input if `echo`, plus prompt if
`prompt`), as it would appear on a terminal.

See [`record()`](https://asciicast.r-lib.org/dev/reference/record.md)
for additional options.
