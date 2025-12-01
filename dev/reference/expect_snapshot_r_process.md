# testthat snapshot test with asciicast

This function is very similar to
[`testthat::expect_snapshot_output()`](https://testthat.r-lib.org/reference/expect_snapshot_output.html),
but it runs the code in an asciciast subprocess, using
[`record_output()`](https://asciicast.r-lib.org/dev/reference/record_output.md).

## Usage

``` r
expect_snapshot_r_process(
  ...,
  interactive = TRUE,
  echo = TRUE,
  startup = NULL,
  transform = NULL,
  variant = NULL
)
```

## Arguments

- ...:

  Code to run (unnamed arguments) and arguments to pass to
  [`record_output()`](https://asciicast.r-lib.org/dev/reference/record_output.md)
  (named arguments). The code is evaluated in a new asciicast
  subprocess. Their output is returned and used in a testthat snapshot
  test.

- interactive:

  Whether to use an interactive R process to evaluate the code.

- echo:

  Whether to echo the code in the subprocess before running it.

- startup:

  Expression to evaluate in the subprocess before recording the
  snapshot. By default it loads and attaches the calling package,
  including its internal functions.

- transform:

  Passed to
  [`testthat::expect_snapshot()`](https://testthat.r-lib.org/reference/expect_snapshot.html).

- variant:

  Passed to
  [`testthat::expect_snapshot()`](https://testthat.r-lib.org/reference/expect_snapshot.html).

## Details

THe `Code` part of the snapshot is always the same, but the `Output`
part shows the code, assuming `echo = TRUE` (the default).

## Examples

``` r
Sys.getpid()
#> [1] 6688
testthat::local_edition(3)
expect_snapshot_r_process(Sys.getpid())
#> ── Snapshot ───────────────────────────────────────────────────────────
#> ℹ Can't save or compare to reference when testing interactively.
#> Code
#>   r_process()
#> Output
#>   > Sys.getpid()
#>   [1] 7189
#> ───────────────────────────────────────────────────────────────────────
```
