# asciicast 2.1.0

* The new `write_html()` function can create a themed HTML snapshot of an
  ascii cast. HTML otuput is now the default in pkgdown, for snapshots.
  For animations we still need to use SVG files.

* asciicast can now record casts on R installations that do not have an R
  shared or static library.

* SVG output now looks correct in Firefox with large fonts (#42).

# asciicast 2.0.0

* Completely new `record()` implementation that uses an embedded R
  interpreter in a subprocess. This means that asciicast can now record
  on Windows, and the recordings are of much faster and of higher quiality.

  Another benefit is that simulated typing does not happen in real time,
  but recordings are as fast as the script itself, and the simulated typing
  (if requested) is added after the recording.

  It is also possible to record R scripts that are not valid R code.
  (These will fail of course, but sometimes that's what you want to show.)

  `record()` does not set the `error` option in the asciicast subprocess
  any more, but the output of errors is simply recorded.

* New `record_output()` which returns the recorded output as a character
  vector instead of an asciic cast.

* Adding a `#'` to a line omits the simulated typing for that line and the
  line will appear in one step.

* New `write_gif()` function to save a cast as a GIF. It needs phantom.js
  currently.

* SVG output can have themes now. The package comes with a bunch of
  predefined themes, or you can create your own. See `default_theme()`.

* knitr options are set temporarily now, and they are restored after the
  knitr run.

* The `asciinema_player()` HTML widget now properly sets the height and
  width of the player window, using the `height` and `width` parameters
  of the cast.

* `record()` now works with script files that end with a comment line (#13).

* The initial R prompt is now not left out when recording a cast.

* `asciinema_player()` now creates an HTML widget that works well on
  Firefox as well.

* asciicast is properly tested now, so it should be much more stable.

# asciicast 1.0.0

First release on CRAN.
