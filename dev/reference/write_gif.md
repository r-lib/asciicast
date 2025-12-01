# Export ascii screencast to animated GIF file

**\[deprecated\]**

## Usage

``` r
write_gif(
  cast,
  path,
  show = NULL,
  cols = NULL,
  rows = NULL,
  theme = NULL,
  scale = 2,
  speed = 1,
  max_colors = 256,
  loop = 0,
  end_wait = 10,
  optimize = TRUE
)
```

## Arguments

- cast:

  `asciicast` object.

- path:

  Path to GIF file to create.

- show:

  Whether to show the GIF on the screen, in the viewer pane in RStudio,
  or using the image viewer in the magick package. By default it only
  show the image in RStudio.

- cols:

  If not `NULL`, *clip* terminal width to this number of columns.

- rows:

  If not `NULL`, *clip* terminal height to this number of rows.

- theme:

  Theme. Currently supported themes: asciinema, tango, solarized-dark,
  solarized-light, monokai. Defaults to the theme specified in the cast,
  or asciiname if not specified.

- scale:

  Image scale / pixel density.

- speed:

  Playback speed. Higher number means faster.

- max_colors:

  Maximum number of colors in the GIF. This is currently per frame.

- loop:

  How many times to loop the animation. Zero means infinite loop.

- end_wait:

  Number of seconds to wait at the end, before looping.

- optimize:

  Whether to try to create smaller GIF files. This might be slow for
  casts with many frames.

## Value

`path`, invisibly.
