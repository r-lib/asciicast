
record_internal <- function(lines, timeout, process) {

  to <- as.double(timeout, units = "secs") * 1000

  ptr <- 1
  output <- character()
  con <- attr(process, "sock")

  send_next_command <- function() {
    while (TRUE) {
      if (!processx::conn_is_incomplete(con)) {
        # R exited unexpectedly while evaluating an expression
        return()
      }

      # if need more lines, but there is no more
      if (ptr > length(lines)) {
        throw(new_error("Incomplete asciicast expression"))
      }

      # send a line
      write_line(process, lines[ptr])
      ptr <<- ptr + 1

      # wait until we see the "input" line, here we might read the
      # leftover from the output
      output <<- c(output, wait_for(process, "^rlib$", "^type: input$", to, ptr - 1))

      # wait until we get back a "busy" linem that means that it is running
      output <<- c(output, wait_for(process, "^rlib$", "^busy:", to, ptr - 1))

      # if busy: 1, then the command is running, nothing more to send
      if (length(output) >= 1) {
        msg <- parse_line(output[length(output)])
        if (msg$value == "busy: 1") break
      }

      # otherwise the expression is incomplete, keep sending
    }
  }

  wait_for_done <- function() {
    output <<- c(output, wait_for(process, "^rlib$", "^busy: 0$", to, ptr - 1))
  }

  while (ptr <= length(lines)) {
    send_next_command()
    wait_for_done()
    if (!processx::conn_is_incomplete(con)) break;
  }

  # there might we some more messages in the output, read those
  output <- c(output, read_all(process))

  msgs <- lapply(output, parse_line)
  data <- tibble::tibble(
    time = vapply(msgs, "[[", double(1), "timestamp"),
    type = vapply(msgs, "[[", character(1), "type"),
    data = vapply(msgs, "[[", character(1), "value")
  )

  data
}

wait_for <- function(px, type = "", value = "", timeout = 1000, linum = "???") {
  con <- attr(px, "sock")
  output <- character()
  while (TRUE) {
    ret <- processx::poll(list(con), timeout)
    if (ret[[1]] == "timeout") {
      throw(new_error("asciicast timeout after line ", linum))
    }
    line <- processx::conn_read_lines(con, 1)
    if (length(line)) {
      output[length(output) + 1L] <- line
      msg <- parse_line(line)
      if (grepl(type, msg$type) && grepl(value, msg$value)) break
      if (msg$type == "rlib" && msg$value == "type: read") break
    }
    if (!processx::conn_is_incomplete(con)) break
  }

  output
}

read_all <- function(px) {
  con <- attr(px, "sock")
  output <- character()
  while (TRUE) {
    ret <- processx::poll(list(con), 0)
    if (ret[[1]] == "timeout") break
    line <- processx::conn_read_lines(con, 1)
    if (length(line)) {
      output[[length(output) + 1L]] <- line
    }
    if (!processx::conn_is_incomplete(con)) break
  }
  output
}

record_embedded <- function(lines, typing_speed, timeout, empty_wait,
                            allow_errors, start_wait, end_wait,
                            record_env, startup, echo, speed, process,
                            interactive) {

  # * allow_errors

  px <- process %||% asciicast_start_process(
    startup,
    timeout,
    record_env,
    interactive
  )

  data <- record_internal(lines, timeout, px)

  if (nrow(data) > 0) {
    data$time <- data$time - data$time[1] + start_wait
  }

  if (end_wait > 0) {
    at <- max(data$time, 0) + end_wait
    data <- rbind(
      data,
      list(time = at, type = "rlib", data = "type: wait"),
      list(time = at, type = "o", data = "")
    )
  }

  if (!echo) data <- remove_input(data)

  if (empty_wait != 0) data <- add_empty_wait(data, empty_wait)

  if (echo) data <- adjust_typing_speed(data, typing_speed)

  if (speed != 1.0) {
    data$time <- (data$time - data$time[1]) / speed + data$time[1]
  }

  data
}

#' Start an asciicast background process
#'
#' This is for expert use, if you want to run multiple recordings in the
#' same process.
#'
#' @param startup Quoted language object to run in the subprocess before
#'   starting the recording.
#' @param timeout Idle timeout, in seconds If the R subprocess running
#'   the recording does not answer within this limit, it is killed and the
#'   recording stops.
#' @param record_env Environment variables to set for the R subprocess.
#' @param interactive Whether to run R in interactive mode. Note that in
#'   interactive mode R might ask for terminal input.
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

asciicast_start_process <- function(startup = NULL, timeout = 10,
                                    record_env = NULL, interactive = TRUE) {

  env <- c(
    ASCIICAST = "true",
    R_HOME = Sys.getenv("R_HOME"),
    R_LIBS_USER = Sys.getenv("R_LIBS_USER"),
    R_LIBS_SITE = Sys.getenv("R_LIBS_SITE"),
    RTOOLS40_HOME = Sys.getenv("RTOOLS40_HOME"),
    RTOOLS42_HOME = Sys.getenv("RTOOLS42_HOME"),
    TMPDIR = Sys.getenv("TMPDIR"),
    PATH = if (is_windows()) paste0(R.home("bin"), ";", Sys.getenv("PATH")),
    record_env
  )

  exec_name <- if (.Platform$OS.type == "windows") "rem.exe" else "rem"
  exec_path <- system.file("bin", exec_name, package = "asciicast")
  if (exec_path == "") {
    exec_path <- system.file("src", exec_name, package = "asciicast")
  }

  sock <- processx::conn_create_unix_socket()
  sock_name <- processx::conn_file_name(sock)

  px <- processx::process$new(
    exec_path,
    c(if (interactive) "-i", sock_name),
    env = env,
    stdout = "|",
    stderr = "2>&1"
  )

  attr(px, "sock") <- sock
  attr(px, "sock_name") <- sock_name

  pr <- processx::poll(list(sock), 5000)[[1]]
  if (pr != "connect") {
    throw(new_error("R subprocess did not connect back"))
  }
  processx::conn_accept_unix_socket(sock)

  to <- as.double(timeout, units = "secs") * 1000
  wait_for(px, "^rlib$", "^busy: 0$", timeout = to)
  read_all(px)

  # throw away the output of the startup code
  lines <- c(
    "Sys.setlocale('LC_ALL', 'en_US.UTF-8')",
    "options(cli.num_colors = 256)",
    "options(cli.dynamic = TRUE)",
    "options(cli.ansi = TRUE)",
    if (!is.null(startup)) deparse(startup)
  )
  record_internal(lines, timeout, process = px)

  if (!processx::conn_is_incomplete(sock)) {
    throw(new_error("asciicast process exited while running `startup`"))
  }

  px
}
write_line <- function(px, line) {
  con <- attr(px, "sock")
  line <- paste0(line, "\n")
  leftover <- processx::conn_write(con, charToRaw(line))
  if (length(leftover) > 0) {
    throw(new_error("Internal asciicast error, cannot send input"))
  }
}

#' @importFrom jsonlite fromJSON

parse_line <- function(line) {
  pcs <- fromJSON(line, simplifyVector = FALSE)
  list(timestamp = pcs[[1]], type = pcs[[2]], value = pcs[[3]])
}

remove_input <- function(data) {
  todel <- which(
    data$type == "rlib" & data$data %in% c("type: input", "type: prompt")
  )
  todel <- c(todel, todel + 1)

  if (length(todel)) {
    data <- data[-todel, ]
  }

  data
}

shift <- function(v) {
  if (length(v) == 0) {
    v
  } else {
    c(v[-1], "")
  }
}

add_empty_wait <- function(data, wait) {
  empty <- which(
    data$type == "rlib" & data$data == "type: input" &
    shift(data$type) == "o" & shift(data$data) == "\r\n"
  )

  if (length(empty) == 0) return(data)

  shft <- rep(0, nrow(data))
  shft[empty] <- wait

  data$time <- data$time + cumsum(shft)

  data
}

adjust_typing_speed <- function(data, typing_speed) {
  inp <- which(
    data$type == "rlib" & data$data == "type: input" &
    shift(data$type) == "o" & !grepl("#[!]\\s*$", shift(data$data))
  )

  data$data <- sub("\\s+#[!]\\s*$", "\r\n", data$data)

  if (typing_speed == 0) return(data)
  if (length(inp) == 0) return(data)

  inp <- c(-1, inp)
  ptr <- 2L
  out <- data[integer(), ]

  while (ptr <= length(inp)) {
    # rows in between are added without modification, including input
    from <- inp[ptr - 1] + 2L
    out <- rbind(out, data[from:inp[ptr], ])

    # the next output row is simulated typing
    oidx <- inp[ptr] + 1
    txt <- data$data[oidx]
    txt <- sub("\r\n$", "", txt)
    chars <- strsplit(txt, "")[[1]]
    delay <- cumsum(c(rtime(length(chars), typing_speed), 0))
    typed <- tibble::tibble(
      time = data$time[oidx] + delay,
      type = "o",
      data = c(chars, "\r\n")
    )
    out <- rbind(out, typed)
    data$time <- data$time + delay[length(delay)]

    ptr <- ptr + 1L
  }

  # and the end
  if (inp[length(inp)] < nrow(data) - 1) {
    from <- inp[length(inp)] + 2L
    out <- rbind(out, data[from:nrow(data), ])
  }

  out
}

#' @importFrom stats runif

rtime <- function(n, speed){
  runif(n, min = speed * 0.5, max = speed * 1.5)
}
