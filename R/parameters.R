
#' asciicast parameters
#'
#' You can set asciicast parameters in the header of the recorded R script.
#' The header is in DCF format (see [read.dcf()]), but all lines are prefixed
#' with `#'` comments.
#'
#' The DCF header may specify arbitrary parameters. We list here the
#' parameters that are interpreted by the asciicast functions.
#'
#' Recording parameters:
#'
#' * `allow_errors`: Whether to cast errors properly. If this is set to
#'   `TRUE`, then asciicast overwrites the `"error"` option. Only change
#'   this if you know what you are doing.
#' * `cols`: Width of the terminal, in number of characters.
#' * `empty_wait`: How long to wait for empty lines in the script file,
#'   in seconds.
#' * `end_wait`: Delay at the very end, in seconds.
#' * `env`: Environment variables to include in the case JSON file.
#'   Defaults to `list(TERM = "xterm-256color", SHELL = "/bin/zsh")`.
#' * `idle_time_limit`: Time limit for the cast not printing anything,
#'   in seconds. By default there is no limit.
#' * `record_env`: Environment variables to set for the R subprocess.
#' * `rows`: Height of the terminal, in number of characters.
#' * `start_wait`: Delay at the beginning, in seconds.
#' * `timeout`: Idle timeout, in seconds If the R subprocess running
#'   the recording does not answer within this limit, it is killed and the
#'   recording stops. Update this for slow running code, that produces no
#'   output as it runs.
#' * `timestamp`: Time stamp of the recording, defaults to `Sys.time()`,
#'   this is included in the cast JSON file.
#' * `title`: Title of the cast, this is included in the cast JSON file.
#' * `typing_speed`: Average typing speed, per keypress, in seconds.
#'
#' Asciinema player parameters:
#'
#' * `author`: Author, displayed in the titlebar in fullscreen mode.
#' * `author_img_url`: URL of the author's image, displayed in the
#'   titlebar in fullscreen mode.
#' * `author_url`: URL of the author's homepage/profile. Author name
#'   (author above) is linked to this URL.
#' * `autoplay`: Whether to start playing the cast automatically.
#' * `cols`: Width of the terminal, in number of characters.
#' * `font_size`: Size of terminal font. Possible values: small, medium,
#'   big, any css `font-size` value (e.g. 15px).
#' * `idle_time_limit`: Time limit for the cast not printing anything,
#'   in seconds. By default there is no limit.
#' * `loop`: Whether to loop the playback.
#' * `poster_frame`: Which frame to use (in seconds) as the preview picture.
#' * `poster_text`: Text to use as the preview picture. Defaults to the
#'   title.
#' * `rows`: Height of the terminal, in number of characters.
#' * `speed`: Whether to play slower or faster. 1 is normal speed.
#' * `start_at`: Where to start the playback from, in seconds.
#' * `theme`: Theme to use, currently it has to be a string, one of
#'    `"asciinema"`, `"tango"`, `"solarized-dark"`, `"solarized-light"`,
#'    `"monokai"`. The first one is the default.
#' * `title`: Title of the cast.
#'
#' Parameters for SVG files:
#' * `at`: Timestamp of single frame to render, in seconds.
#' * `cols`: Width of the terminal, in number of characters.
#' * `cursor`: Enable cursor rendering.
#' * `end_at`: Upper range of timeline to render in seconds.
#' * `padding`: Distance between text and image bounds.
#' * `padding_x`: Distance between text and image bounds on x axis.
#' * `padding_y`: Distance between text and image bounds on y axis.
#' * `rows`: Height of the terminal, in number of characters.
#' * `start_at`: Where to start the playback from, in seconds.
#' * `window`: Render with window decorations.
#' * `theme`: Theme to use, currently it has to be a string referring to
#'    a build-in theme, or a named list of theme properties,
#'    see [default_theme()].
#'    The built-in themes are `"asciinema"`, `"tango"`, `"solarized-dark"`,
#'    `"solarized-light"`, `"seti"`, `"monokai"`, `"github-light"`,
#'    `"pkgdown"`.
#'
#' @name asciicast-package
#' @family asciicast functions
NULL
