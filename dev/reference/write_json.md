# Write an ascii cast to file

The file uses the asciinema file format, version 2:
<https://github.com/asciinema/asciinema/blob/master/doc/asciicast-v2.md>.

## Usage

``` r
write_json(cast, path)
```

## Arguments

- cast:

  `asciicast` object.

- path:

  Path to write to.

## See also

Other asciicast functions:
[`asciicast-package`](https://asciicast.r-lib.org/dev/reference/asciicast-package.md),
[`asciicast_start_process()`](https://asciicast.r-lib.org/dev/reference/asciicast_start_process.md),
[`read_cast()`](https://asciicast.r-lib.org/dev/reference/read_cast.md),
[`record()`](https://asciicast.r-lib.org/dev/reference/record.md)

## Examples

``` r
script <- system.file("examples", "hello.R", package = "asciicast")
cast <- record(script)
json <- tempfile(fileext = ".json")
write_json(cast, json)
```
