
record_commands <- function(lines, typing_speed, timeout, empty_wait,
                            allow_errors, start_wait, end_wait,
                            record_env, startup, echo, process) {

  next_line <- 1L
  output <- list()
  typing_speed <- c(typing_speed, 0)
  this_typing_speed <- NULL
  this_callback <- NULL

  start_callback <- function(out) {
    output <<- append(output, list(list(0, out)))
  }

  px <- process
  if (is.null(px)) {
    px <- asciicast_start_process2(
      timeout, allow_errors, startup, record_env, echo, start_callback)
    on.exit({ close(px$get_input_connection()); px$kill() }, add = TRUE)
  }

  start <- Sys.time()

  output_callback <- function(out) {
    output <<- append(output, list(list(Sys.time() - start, out)))
  }

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

  is_command_line <- function(line) {
    grepl("^# <<", line)
  }

  run_command_line <- function(line) {
    line <- str_trim(sub("^# <<", "", line))
    typing_speed <<- rev(typing_speed)
    this_typing_speed <<- typing_speed[1]
  }

  messagex("--> ...")
  poll_wait(px, start_wait, output_callback)

  this_typing_speed <- typing_speed[1]
  this_callback <- output_callback

  while (next_line <= length(lines)) {
    expr <- next_expression()
    for (line in expr) {
      if (is_empty_line(line)) {
        messagex("--> ...")
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
#' @param echo Whether to echo the input to the terminal. If `FALSE`, then
#'   only the output is shown.
#' @return The R process, a [processx::process] object.
#'
#' @family asciicast functions
#' @export
#' @examplesIf asciicast:::is_recording_supported()
#' # Use the same R process to record multiple casts
#' process <- asciicast_start_process()
#' script1 <- "a <- runif(10)\n"
#' script2 <- "a\n"
#' cast1 <- record(textConnection(script1), process = process)
#' cast2 <- record(textConnection(script2), process = process)
#' cast1
#' cast2

asciicast_start_process <- function(timeout = 10, allow_errors = TRUE,
                                    startup = NULL, record_env = NULL,
                                    echo = TRUE) {
  asciicast_start_process2(timeout, allow_errors, startup, record_env,
                           echo, callback = NULL)
}

asciicast_start_process2 <- function(timeout, allow_errors, startup,
                                     record_env = NULL, echo, callback) {

  env <- Sys.getenv()
  env["ASCIICAST"] <- "true"
  env[names(record_env)] <- record_env

  exec_name <- if (.Platform$OS.type == "windows") "Rterm" else "R"
  R <- file.path(R.home("bin"), exec_name)
  px <- processx::process$new(R, c("-q", if (!echo) "--slave"),
                              pty = TRUE, pty_options = list(echo = echo),
                              poll_connection = TRUE, env = env)

  # If there is no echo, then we need to know if the process is ready
  # somehow, so we just write some dummy input, and then we also read
  # out its output. In theory it is not guaranteed that we read out all
  # output, but in practive we always do. If echo == TRUE, then all this
  # is not needed, because R will produce some output.
  if (!echo) px$write_input("1\n")
  ready <- px$poll_io(5000)
  if (!px$is_alive() || ready["output"] != "ready") {
    stop("R subprocess is not ready after 5s")
  }
  if (!echo) px$read_output()
  record_setup_subprocess(px, timeout, allow_errors, startup, callback)

  px
}

#' @importFrom stats runif

rtime <- function(n, speed){
  runif(n, min = speed * 0.5, max = speed * 1.5)
}

type_input <- function(proc, text, speed, callback) {
  messagex("--> ", appendLF = FALSE)
  if (speed == 0) {
    write_for_sure(proc, text)
    messagex(text, appendLF = FALSE)
  } else {
    chars <- strsplit(text, "")[[1]]
    time <- rtime(length(chars), speed)
    for (i in seq_along(chars)) {
      write_for_sure(proc, chars[i])
      messagex(chars[i], appendLF = FALSE)
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

#' @importFrom uuid UUIDgenerate

record_setup_subprocess <- function(proc, timeout, allow_errors, startup,
                                    callback) {
  id <- UUIDgenerate()
  substs <- list(
    allow_errors = allow_errors,
    env_file = env_file,
    startup = startup,
    id = id)

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
    messagex(id, " done")
  }, substs)

  setupstr <- paste0(deparse(setup), "\n", collapse = "")
  write_for_sure(proc, setupstr)

  ## Wait until 'id' shows up in the output, and after that use the
  ## proper callback
  done <- FALSE
  output <- ""
  str <- paste0(id, " done\r\n")
  wait_for_done(proc, timeout, function(out) {
    if (done) {
      if (!is.null(callback)) callback(out)
      return()
    }
    output <<- paste0(output, out)
    if (grepl(str, output, fixed = TRUE, useBytes = TRUE)) {
      out <- strsplit(output, str, fixed = TRUE)[[1]][2]
      done <<- TRUE
      if (!is.na(out) && !is.null(callback)) callback(out)
    }
  })
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
