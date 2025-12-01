# Package index

## Basics

- [`record()`](https://asciicast.r-lib.org/dev/reference/record.md) :
  Record an asciinema screencast
- [`play()`](https://asciicast.r-lib.org/dev/reference/play.md) : Play
  asciinema cast as an SVG image in the default browser
- [`merge_casts()`](https://asciicast.r-lib.org/dev/reference/merge_casts.md)
  [`clear_screen()`](https://asciicast.r-lib.org/dev/reference/merge_casts.md)
  [`pause()`](https://asciicast.r-lib.org/dev/reference/merge_casts.md)
  : Merge multiple ASCII casts into one
- [`record_output()`](https://asciicast.r-lib.org/dev/reference/record_output.md)
  : Record output of an R script and return it as a character vector

## Sharing

- [`write_svg()`](https://asciicast.r-lib.org/dev/reference/write_svg.md)
  : Create animated SVG from an asciicast
- [`write_html()`](https://asciicast.r-lib.org/dev/reference/write_html.md)
  : Create a HTML snapshot of an asciicast
- [`write_gif()`](https://asciicast.r-lib.org/dev/reference/write_gif.md)
  **\[deprecated\]** : Export ascii screencast to animated GIF file
- [`write_json()`](https://asciicast.r-lib.org/dev/reference/write_json.md)
  : Write an ascii cast to file
- [`read_cast()`](https://asciicast.r-lib.org/dev/reference/read_cast.md)
  : Import an asciicast from an asciicast JSON file

## Theming and options

- [`default_theme()`](https://asciicast.r-lib.org/dev/reference/default_theme.md)
  : The default asciicast theme
- [`asciicast_options()`](https://asciicast.r-lib.org/dev/reference/asciicast_options.md)
  : Default options to set in the asciicast subprocess.
- [`asciicast-package`](https://asciicast.r-lib.org/dev/reference/asciicast-package.md)
  : asciicast parameters

## Other functions

- [`get_locales()`](https://asciicast.r-lib.org/dev/reference/get_locales.md)
  : Helper function to query locales as a named character vector.
- [`init_knitr_engine()`](https://asciicast.r-lib.org/dev/reference/init_knitr_engine.md)
  : Initialize the asciicast knitr engine
- [`asciicast_start_process()`](https://asciicast.r-lib.org/dev/reference/asciicast_start_process.md)
  : Start an asciicast background process
- [`asciinema_player()`](https://asciicast.r-lib.org/dev/reference/asciinema_player.md)
  : asciinema player HTML widget
- [`expect_snapshot_r_process()`](https://asciicast.r-lib.org/dev/reference/expect_snapshot_r_process.md)
  : testthat snapshot test with asciicast
- [`install_phantomjs()`](https://asciicast.r-lib.org/dev/reference/install_phantomjs.md)
  : Install PhantomJS
