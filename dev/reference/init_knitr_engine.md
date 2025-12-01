# Initialize the asciicast knitr engine

Call this function in your Rmd file to enable creating asciinema casts
from code chunks.

## Usage

``` r
init_knitr_engine(
  echo = FALSE,
  same_process = TRUE,
  timeout = 10,
  startup = NULL,
  record_env = NULL,
  echo_input = TRUE,
  interactive = TRUE
)
```

## Arguments

- echo:

  Whether to print the code of asciicast chunks.

- same_process:

  Whether to run all asciicast chunks *in the same* R process. To
  restart this R process, call `init_knitr_engine()` again.

- timeout:

  Idle timeout, in seconds If the R subprocess running the recording
  does not answer within this limit, it is killed and the recording
  stops.

- startup:

  Quoted language object to run in the subprocess before starting the
  recording.

- record_env:

  Environment variables to set for the R subprocess.

- echo_input:

  Whether to echo the input in the asciicast recording.

- interactive:

  Whether to run R in interactive mode. Note that in interactive mode R
  might ask for terminal input.

## Details

### Limitations

- `purl()` or setting the `purl = TRUE` chunk option, does not work
  properly, in that knitr thinks that asciicast chunks are not R code,
  so they will appear as comments. If you know how to fix this, please
  contact us.

## Examples

Call this function from an Rmd chunk and then you can use the asciicast
knitr engine:

    ```{r setup, include = FALSE}
    asciicast::init_knitr_engine()
    ```

    ```{asciicast, cache = TRUE}`
    #' Rows: 10
    # This is an asciicast example
    loadedNamespaces()
    ```
