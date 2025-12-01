# Record an asciinema screencast

Record an asciinema screencast

## Usage

``` r
record(
  script,
  typing_speed = NULL,
  empty_wait = NULL,
  cols = NULL,
  rows = NULL,
  title = NULL,
  timestamp = NULL,
  env = NULL,
  idle_time_limit = NULL,
  timeout = NULL,
  start_wait = NULL,
  end_wait = NULL,
  record_env = NULL,
  startup = NULL,
  echo = TRUE,
  speed = NULL,
  process = NULL,
  interactive = TRUE,
  locales = get_locales(),
  options = asciicast_options(),
  incomplete_error = NULL,
  show_output = FALSE
)
```

## Arguments

- script:

  Path of an R script to record. It can also be a readable R connection
  or URL, as it is passed to
  [`base::readLines()`](https://rdrr.io/r/base/readLines.html). It can
  also be a language object, which is deparsed, or a character vector
  with the source code itself.

- typing_speed:

  Average typing speed, per keypress, in seconds.

- empty_wait:

  How long to wait for empty lines in the script file, in seconds.

- cols:

  Width of the terminal, in number of characters.

- rows:

  Height of the terminal, in number of characters. If it the string
  `"auto"`, then it will be determined automatically, by including all
  output on the screen.

- title:

  Title of the cast, this is included in the cast JSON file.

- timestamp:

  Time stamp of the recording, defaults to
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html), this is included
  in the cast JSON file.

- env:

  Environment variables to include in the case JSON file. Defaults to
  `list(TERM = "xterm-256color", SHELL = "/bin/zsh")`.

- idle_time_limit:

  Time limit for the cast not printing anything, in seconds. By default
  there is no limit.

- timeout:

  Idle timeout, in seconds If the R subprocess running the recording
  does not answer within this limit, it is killed and the recording
  stops. Update this for slow running code, that produces no output as
  it runs.

- start_wait:

  Delay at the beginning, in seconds.

- end_wait:

  Delay at the very end, in seconds.

- record_env:

  Environment variables to set for the R subprocess.

- startup:

  Quoted language object to run in the subprocess before starting the
  recording.

- echo:

  Whether to echo the input to the terminal. If `FALSE`, then only the
  output is shown.

- speed:

  Rescale the speed of the recorded cast with this factor. The delay of
  the first frame is kept constant.

- process:

  A processx subprocess to run the cast in. By default a new subprocess
  is started. You can reuse a process by calling
  [`asciicast_start_process()`](https://asciicast.r-lib.org/dev/reference/asciicast_start_process.md)
  first, and supplying the returned process here.

- interactive:

  Whether to run R in interactive mode. This argument is ignored if
  `process` is specified. If `process` is `NULL` then it is passed to
  [`asciicast_start_process()`](https://asciicast.r-lib.org/dev/reference/asciicast_start_process.md).

- locales:

  Locales to set in the asciicast subprocess. Defaults to the current
  locales in the main R process. Specify a named character vector here
  to override some of the defaults. See also
  [`get_locales()`](https://asciicast.r-lib.org/dev/reference/get_locales.md).

- options:

  Options to set in the subprocess, a named list. They are deparsed to
  code, and then the code setting them is executed in the subprocess.
  See
  [`asciicast_options()`](https://asciicast.r-lib.org/dev/reference/asciicast_options.md)
  for the defaults. Supply a named list here to override the defaults or
  set additionsl ones. Passing large and/or complicated options here
  might not work, or might be slow.

- incomplete_error:

  Whether to error on incomplete expressions. You might need to set this
  to `FALSE` for R code that does keyboard input, e.g. in
  [`browser()`](https://rdrr.io/r/base/browser.html). The default is
  `TRUE`.

- show_output:

  Whether to show the output of the subprocess in real time.

## Value

An `asciicast` object, write this to file with
[`write_json()`](https://asciicast.r-lib.org/dev/reference/write_json.md).

## See also

Other asciicast functions:
[`asciicast-package`](https://asciicast.r-lib.org/dev/reference/asciicast-package.md),
[`asciicast_start_process()`](https://asciicast.r-lib.org/dev/reference/asciicast_start_process.md),
[`read_cast()`](https://asciicast.r-lib.org/dev/reference/read_cast.md),
[`write_json()`](https://asciicast.r-lib.org/dev/reference/write_json.md)

## Examples

``` r
if (FALSE) { # interactive()
script <- system.file("examples", "hello.R", package = "asciicast")
cast <- record(script)
play(cast)
}
```
