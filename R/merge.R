
new_merge_cast_command <- function(command, output) {
  structure(
    list(command = command, output = list(output = output)),
    class = "merge_cast_command"
  )
}

#' @rdname merge_casts
#' @export

clear_screen <- function() {
  output <- tibble::tibble(
    time = 0,
    type = "o",
    data = "\u001b[H\u001b[2J"
  )
  new_merge_cast_command("clear_screen", output = output)
}

#' @rdname merge_casts
#' @param secs Number of seconds to wait.
#' @export

pause <- function(secs) {
  output <- tibble::tibble(
    time = as.numeric(secs),
    type = "o",
    data = ""
  )
  new_merge_cast_command("pause", output = output)
}

handle_merge_cast <- function(x) {
  if (inherits(x, "merge_cast_command")) {
    list(type = "command", output = x$output)
  } else if (inherits(x, "asciicast")) {
    list(type = "cast", output = x)
  } else {
    list(type = "cast", output = read_cast(x))
  }
}

#' Merge multiple ASCII casts into one
#'
#' The new cast will inherit its options (screen size, etc.) from the
#' first cast in the argument list. The options of the rest of the casts
#' are ignored.
#'
#' `pause()` inserts a pause of the specified seconds between the casts.
#'
#' `clear_screen()` clears the screen between two casts.
#'
#' @param ... Ascii casts to merge or merge commands. Merge commands
#'   provide a way to insert pause, clear the screen, etc., between casts.
#' @return An `asciicast` object.
#'
#' @export
#' @examplesIf interactive()
#' # merge two casts, with a pause, and clear screen between them
#' cast1 <- read_cast(system.file("examples", "hello.cast", package = "asciicast"))
#' cast2 <- read_cast(system.file("examples", "dplyr.cast", package = "asciicast"))
#' cast <- merge_casts(cast1, pause(3), clear_screen(), cast2)
#' play(cast)

merge_casts <- function(...) {
  casts <- lapply(list(...), handle_merge_cast)
  types <- map_chr(casts, "[[", "type")
  if (! "cast" %in% types) {
    stop("You need to include at least one cast in `merge_cast()`.")
  }
  wconf <- which(types == "cast")[[1]]
  new_cast <- structure(
    list(
      config = casts[[wconf]]$output$config,
      output = shift_output(lapply(casts, function(x) x$output$output))
    ),
    class = "asciicast"
  )
}

shift_output <- function(recs) {
  last <- map_dbl(recs, function(x) utils::tail(x$time, 1)) + 1/10000
  shift <- utils::head(cumsum(c(0, last)), -1)
  for (i in seq_along(recs)) {
    recs[[i]]$time <- recs[[i]]$time + shift[[i]]
  }
  do.call(rbind, recs)
}
