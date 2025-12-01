# Import an asciicast from an asciicast JSON file

Import an asciicast from an asciicast JSON file

## Usage

``` r
read_cast(json)
```

## Arguments

- json:

  Path to JSON asciicast file, version 2:
  <https://github.com/asciinema/asciinema/blob/master/doc/asciicast-v2.md>.
  If a numeric id, then it is taken as a public <https://asciinema.org>
  recording id, that is downloaded. It can also be a URL of private
  <https://asciinema.org> link.

## Value

`asciicast` object.

## See also

Other asciicast functions:
[`asciicast-package`](https://asciicast.r-lib.org/dev/reference/asciicast-package.md),
[`asciicast_start_process()`](https://asciicast.r-lib.org/dev/reference/asciicast_start_process.md),
[`record()`](https://asciicast.r-lib.org/dev/reference/record.md),
[`write_json()`](https://asciicast.r-lib.org/dev/reference/write_json.md)

## Examples

``` r
if (FALSE) { # interactive()
c1 <- read_cast("https://asciinema.org/a/uHQwIVpiZvu0Ioio8KYx6Uwlj.cast?dl=1")
play(c1)

c2 <- read_cast(258660)
play(c2)
}
if (FALSE) { # interactive()
c3 <- read_cast(system.file("examples", "hello.cast", package = "asciicast"))
play(c3)
}
```
