# asciinema player HTML widget

You can use this widget in Rmd files or Shiny applications, the same way
as [other HTML widgets](http://www.htmlwidgets.org/).

## Usage

``` r
asciinema_player(
  cast,
  start_at = 0,
  rows = NULL,
  cols = NULL,
  autoplay = NULL,
  loop = NULL,
  speed = NULL,
  title = NULL,
  author = NULL,
  author_url = NULL,
  author_img_url = NULL,
  poster_text = NULL,
  poster_frame = NULL,
  font_size = NULL,
  theme = NULL,
  idle_time_limit = NULL,
  html_height = NULL,
  html_width = NULL,
  element_id = NULL
)
```

## Arguments

- cast:

  `asciicast` object.

- start_at:

  Where to start the playback from, in seconds.

- rows:

  Number of rows, defaults to the number of rows in the recording, or 24
  if not specified in the cast.

- cols:

  Number of columns, defaults to the number columns in the recording, or
  80 if not specified in the cast.

- autoplay:

  Whether to start playing the cast automatically.

- loop:

  Whether to loop the playback.

- speed:

  Whether to play slower or faster. 1 is normal speed.

- title:

  If specified, it overrides the title in the recording.

- author:

  Author, displayed in the titlebar in fullscreen mode.

- author_url:

  URL of the author's homepage/profile. Author name (author above) is
  linked to this URL.

- author_img_url:

  URL of the author's image, displayed in the titlebar in fullscreen
  mode.

- poster_text:

  if not `NULL`, used as the text of the poster (preview).

- poster_frame:

  Which frame to use for the preview. A number means seconds. Defaults
  to the last frame. This is only used if `poster_text` is `NULL`.

- font_size:

  Size of terminal font. Possible values: small, medium, big, any css
  `font-size` value (e.g. 15px).

- theme:

  Theme.

- idle_time_limit:

  Time limit for the cast not printing anything, in seconds. By default
  there is no limit.

- html_height:

  HTML height of the widget.

- html_width:

  HTML width of the widget.

- element_id:

  HTML id of the widget's element. If `NULL`, then the id is generated
  randomly.

## Examples

``` r
if (FALSE) { # interactive()
cast <- read_cast(system.file("examples", "hello.cast", package = "asciicast"))
asciinema_player(cast)
}
```
