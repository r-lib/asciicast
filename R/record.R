
record_commands <- function(lines, typing_speed, timeout, empty_wait,
                            allow_errors, start_wait, end_wait,
                            record_env, startup, process) {

  px <- process
  if (is.null(px)) {
    px <- asciicast_start_process(timeout, allow_errors, startup, record_env)
    on.exit({ close(px$get_input_connection()); px$kill() }, add = TRUE)
  }

  start <- Sys.time()
  next_line <- 1L
  output <- list()
  typing_speed <- c(typing_speed, 0)
  this_typing_speed <- NULL
  this_callback <- NULL

  next_expression <- function() {
    end <- next_line
    while (end <= length(lines) && ! is_complete(lines[next_line:end])) {
      end <- end + 1L
    }

    if (end > length(lines)) {
      stop("Incomplete expression at end of file, from line ", next_line)
    }

    expr <- lines[next_line:end]
    next_line <<- end + 1L
    expr
  }

  output_callback <- function(out) {
    output <<- append(output, list(list(Sys.time() - start, out)))
  }

  is_command_line <- function(line) {
    grepl("^# <<", line)
  }

  run_command_line <- function(line) {
    line <- str_trim(sub("^# <<", "", line))
    typing_speed <<- rev(typing_speed)
    this_typing_speed <<- typing_speed[1]
  }

  cat("--> ...\n")
  poll_wait(px, start_wait, output_callback)

  this_typing_speed <- typing_speed[1]
  this_callback <- output_callback

  while (next_line <= length(lines)) {
    expr <- next_expression()
    for (line in expr) {
      if (is_empty_line(line)) {
        cat("--> ...\n")
        type_input(px, "\n", 0L, this_callback)
        poll_wait(px, empty_wait, this_callback)
        this_typing_speed <- typing_speed[1]
        this_callback <- output_callback
      } else if (is_command_line(line)) {
        run_command_line(line)
      } else {
        linenl <- paste0(line, "\n")
        type_input(px, linenl, this_typing_speed, this_callback)
      }
    }
    wait_for_done(px, timeout, this_callback)
  }

  if (is.null(process)) {
    close(px$get_input_connection())
    poll_wait(px, timeout, output_callback, done = TRUE)
    px$wait(timeout)
    if (px$is_alive()) stop("R subprocess did not finish")
  }

  output <- append(output, list(list(Sys.time() - start + end_wait, "")))

  tibble::tibble(
    time = as.double(vapply(output, "[[", double(1), 1), units = "secs"),
    type = "o",
    data = vapply(output, "[[", character(1), 2))
}

#' Start an asciicast background process
#'
#' This is for expert use, if you want to run multiple recordings in the
#' same process.
#'
#' @param timeout Idle timeout, in seconds If the R subprocess running
#'   the recording does not answer within this limit, it is killed and the
#'   recording stops. Update this for slow running code, that produces no
#'   output as it runs.
#' @param allow_errors Whether to cast errors properly. If this is set to
#'   `TRUE`, then asciicast overwrites the `"error"` option. Only change
#'   this if you know what you are doing.
#' @param startup Quoted language object to run in the subprocess before
#'   starting the recording.
#' @param record_env Environment variables to set for the R subprocess.
#'
#' @family asciicast functions
#' @export

asciicast_start_process <- function(timeout = 10, allow_errors = TRUE,
                                    startup = NULL, record_env = NULL) {
  env <- Sys.getenv()
  env["ASCIICAST"] <- "true"
  env[names(record_env)] <- record_env

  px <- processx::process$new("R", "-q", pty = TRUE,
                              pty_options = list(echo = TRUE),
                              poll_connection = TRUE, env = env)

  ready <- px$poll_io(5000)
  if (!px$is_alive() || ready["output"] != "ready") {
    stop("R subprocess is not ready after 5s")
  }
  record_setup_subprocess(px, timeout, allow_errors, startup)

  px
}

#' @importFrom stats runif

rtime <- function(n, speed){
  runif(n, min = speed * 0.5, max = speed * 1.5)
}

type_input <- function(proc, text, speed, callback) {
  cat("--> ")
  if (speed == 0) {
    write_for_sure(proc, text)
    cat(text)
  } else {
    chars <- strsplit(text, "")[[1]]
    time <- rtime(length(chars), speed)
    for (i in seq_along(chars)) {
      write_for_sure(proc, chars[i])
      cat(chars[i])
      poll_wait(proc, time[i], callback)
    }
  }
}

# Wait for the specified amount of time, but still read the output
# while waiting

poll_wait <- function(proc, time, callback = NULL, done = FALSE) {
  deadline <- Sys.time() + time
  while ((left <- deadline - Sys.time()) > 0) {
    timeout <- as.double(left, units = "secs") * 1000
    ready <- proc$poll_io(as.integer(timeout))
    if (ready["output"] == "ready" && !is.null(callback)) {
      callback(proc$read_output())
    }
    if (done && ready["process"] == "ready") return()
  }
}

record_setup_subprocess <- function(proc, timeout, allow_errors, startup) {
  setup <- substitute({
    while ("tools:asciicast" %in% search()) detach("tools:asciicast")
    env <- readRDS(env_file)
    do.call(
      "attach",
      list(env, pos = length(search()), name = "tools:asciicast"))
    data <- env$`__asciicast_data__`
    data$pxlib <- data$load_client_lib(data$sofile)
    addTaskCallback(function(...) {
      env <- as.environment("tools:asciicast")$`__asciicast_data__`
      env$pxlib$write_fd(3L, "OK\n")
      TRUE
    })
    if (allow_errors) {
      options(error = function() {
        env <- as.environment("tools:asciicast")$`__asciicast_data__`
        env$pxlib$write_fd(3L, "OK\n")
      })
    }
    rm(env, data)
    startup
  }, list(allow_errors = allow_errors, env_file = env_file, startup = startup))

  setupstr <- paste0(deparse(setup), "\n", collapse = "")
  write_for_sure(proc, setupstr)
  wait_for_done(proc, timeout)
}

write_for_sure <- function(proc, text) {
  while (1) {
    text <- proc$write_input(text)
    if (!length(text)) break;
    Sys.sleep(.1)
  }
}

wait_for_done <- function(proc, timeout, callback = NULL) {
  while (1) {
    ready <- proc$poll_io(timeout * 1000)
    if (ready["output"] == "ready") {
      out <- proc$read_output()
      if (!is.null(callback)) callback(out)
    } else {
      break
    }
  }
  ready <- proc$poll_io(timeout * 1000)
  if (ready["process"] != "ready") stop("R subprocess did not respond")
  con <- proc$get_poll_connection()
  processx::conn_read_lines(con, n = 1)
}

is_complete <- function(x) {
  err <- expr <- NULL
  tryCatch(expr <- parse(text = x), error = function(e) err <<- e)

  # Might be an empty line or a comment, they are considered incomplete
  if (length(expr) == 0) return(FALSE)

  # Otherwise if no error, then we are good
  if (is.null(err)) return(TRUE)

  # If error, then need to check if "unexpected end of input",
  # because that is incomplete. If a parse error, then it is complete.
  exp <- tryCatch(parse(text = "1+"), error = function(e) e$message)
  exp1 <- strsplit(exp, "\n")[[1]][[1]]
  msg <- sub("^.*:\\s*([^:]+)$",  "\\1", exp1, perl = TRUE)
  ! grepl(msg, conditionMessage(err), fixed = TRUE)
}
