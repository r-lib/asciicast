# Merge multiple ASCII casts into one

The new cast will inherit its options (screen size, etc.) from the first
cast in the argument list. The options of the rest of the casts are
ignored.

## Usage

``` r
merge_casts(...)

clear_screen()

pause(secs)
```

## Arguments

- ...:

  Ascii casts to merge or merge commands. Merge commands provide a way
  to insert pause, clear the screen, etc., between casts.

- secs:

  Number of seconds to wait.

## Value

An `asciicast` object.

## Details

`pause()` inserts a pause of the specified seconds between the casts.

`clear_screen()` clears the screen between two casts.

## Examples

``` r
if (FALSE) { # interactive()
# merge two casts, with a pause, and clear screen between them
cast1 <- read_cast(system.file("examples", "hello.cast", package = "asciicast"))
cast2 <- read_cast(system.file("examples", "dplyr.cast", package = "asciicast"))
cast <- merge_casts(cast1, pause(3), clear_screen(), cast2)
play(cast)
}
```
