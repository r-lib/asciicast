# Start an asciicast background process

This is for expert use, if you want to run multiple recordings in the
same process.

## Usage

``` r
asciicast_start_process(
  startup = NULL,
  timeout = 10,
  record_env = NULL,
  interactive = TRUE,
  locales = get_locales(),
  options = NULL,
  show_output = FALSE
)
```

## Arguments

- startup:

  Quoted language object to run in the subprocess before starting the
  recording.

- timeout:

  Idle timeout, in seconds If the R subprocess running the recording
  does not answer within this limit, it is killed and the recording
  stops.

- record_env:

  Environment variables to set for the R subprocess.

- interactive:

  Whether to run R in interactive mode. Note that in interactive mode R
  might ask for terminal input.

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

- show_output:

  Whether to show the output of the subprocess in real time.

## Value

The R process, a
[processx::process](http://processx.r-lib.org/reference/process.md)
object.

## See also

Other asciicast functions:
[`asciicast-package`](https://asciicast.r-lib.org/dev/reference/asciicast-package.md),
[`read_cast()`](https://asciicast.r-lib.org/dev/reference/read_cast.md),
[`record()`](https://asciicast.r-lib.org/dev/reference/record.md),
[`write_json()`](https://asciicast.r-lib.org/dev/reference/write_json.md)

## Examples

``` r
# Use the same R process to record multiple casts
process <- asciicast_start_process()
script1 <- "a <- runif(10)\n"
script2 <- "a\n"
cast1 <- record(textConnection(script1), process = process)
cast2 <- record(textConnection(script2), process = process)
cast1
#> <asciicast>
#> <config>
#>   "version": 2,
#>   "command": "R -q",
#>   "timestamp": 1764586914,
#>   "env": {
#>     "TERM": "xterm-256color",
#>     "SHELL": "/bin/zsh"
#>   },
#>   "height": 24,
#>   "rows": 24,
#>   "width": 80,
#>   "cols": 80
#> 
#> <frames>
#> # A tibble: 24 × 3
#>      time type  data                
#>     <dbl> <chr> <chr>               
#>  1 0      rlib  "type: prompt"      
#>  2 0      o     "> "                
#>  3 0      i     "a <- runif(10)\r\n"
#>  4 0      rlib  "type: input"       
#>  5 0.0254 o     "a"                 
#>  6 0.0737 o     " "                 
#>  7 0.124  o     "<"                 
#>  8 0.163  o     "-"                 
#>  9 0.225  o     " "                 
#> 10 0.288  o     "r"                 
#> # ℹ 14 more rows
cast2
#> <asciicast>
#> <config>
#>   "version": 2,
#>   "command": "R -q",
#>   "timestamp": 1764586914,
#>   "env": {
#>     "TERM": "xterm-256color",
#>     "SHELL": "/bin/zsh"
#>   },
#>   "height": 24,
#>   "rows": 24,
#>   "width": 80,
#>   "cols": 80
#> 
#> <frames>
#> # A tibble: 39 × 3
#>      time type  data          
#>     <dbl> <chr> <chr>         
#>  1 0      rlib  "type: prompt"
#>  2 0      o     "> "          
#>  3 0      i     "a\r\n"       
#>  4 0      rlib  "type: input" 
#>  5 0.0444 o     "a"           
#>  6 0.0444 o     "\r\n"        
#>  7 0.0445 rlib  "busy: 1"     
#>  8 0.0445 rlib  "type: stdout"
#>  9 0.0445 o     " [1]"        
#> 10 0.0445 rlib  "type: stdout"
#> # ℹ 29 more rows
```
