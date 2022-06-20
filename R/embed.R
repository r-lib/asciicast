
record_internal <- function(lines, timeout, process) {

  timeout <- as.double(timeout, units = "secs") * 1000

  ptr <- 1
  output <- character()
  px <- process
  con <- attr(px, "cast_fifo")

  send_next_command <- function() {
    while (TRUE) {

      # if need more lines, but there is no more
      if (ptr > length(lines)) {
        throw(new_error("Incomplete asciicast expression"))
      }

      # send a line
      write_line(px, lines[ptr])
      ptr <<- ptr + 1

      # wait until we see the "input" line, here we might read the
      # leftover from the output
      while (TRUE) {
        ret <- processx::poll(list(con), timeout)
        if (ret[[1]] == "timeout") {
          throw(new_error("asciicast timeout at line ", ptr - 1))
        }
        line <- read_line(px)
        output[length(output) + 1L] <<- line
        msg <- parse_line(line)
        if (msg$type == "rlib" && msg$value == "type: input") break
      }

      # wait until we get back a "busy" linem that means that it is running
      while (TRUE) {
        ret <- processx::poll(list(con), timeout)
        if (ret[[1]] == "timeout") {
          throw(new_error("asciicast timeout at line ", ptr - 1))
        }
        line <- read_line(px)
        output[length(output) + 1L] <<- line
        msg <- parse_line(line)
        if (msg$type == "rlib" && grepl("^busy:", msg$value)) break
      }

      # if busy: 1, then the command is running, nothing more to send
      if (msg$value == "busy: 1") break

      # otherwise the expression is incomplete, keep sending
    }
  }

  wait_for_done <- function() {
    while (TRUE) {
      ret <- processx::poll(list(con), timeout)
      if (ret[[1]] == "timeout") {
        throw(new_error("asciicast timeout at line ", ptr - 1))
      }
      line <- read_line(px)
      output[length(output) + 1L] <<- line
      msg <- parse_line(line)
      if (msg$type == "rlib" && msg$value == "busy: 0") break
    }
  }

  while (ptr <= length(lines)) {
    send_next_command()
    wait_for_done()
  }

  # there might we some more messages in the output, read those
  while (TRUE) {
    ret <- processx::poll(list(con), 0)
    if (ret[[1]] == "timeout") break
    line <-read_line(px)
    output[[length(output) + 1L]] <- line
  }

  msgs <- lapply(output, parse_line)
  data <- tibble::tibble(
    time = vapply(msgs, "[[", double(1), "timestamp"),
    type = vapply(msgs, "[[", character(1), "type"),
    data = vapply(msgs, "[[", character(1), "value")
  )

  data
}

record_embedded <- function(lines, typing_speed, timeout, empty_wait,
                            allow_errors, start_wait, end_wait,
                            record_env, startup, echo, speed, process) {

  # * allow_errors

  px <- process %||% asciicast_start_process(startup, timeout, record_env)

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
                                    record_env = NULL) {

  env <- c(
    ASCIICAST = "true",
    R_HOME = Sys.getenv("R_HOME"),
    TMPDIR = Sys.getenv("TMPDIR"),
    record_env
  )

  exec_name <- if (.Platform$OS.type == "windows") "rem.exe" else "rem"
  exec_path <- system.file("bin", exec_name, package = "asciicast")
  if (exec_path == "") {
    exec_path <- system.file("src", exec_name, package = "asciicast")
  }

  make_fifo(input_fifo <- tempfile("asciicast"))
  make_fifo(cast_fifo <- tempfile("asciicast"))

  px <- processx::process$new(
    exec_path,
    c(input_fifo, cast_fifo),
    env = env,
    stdout = "|",
    stderr = "|"
  )

  attr(px, "input_fifo_path") <- input_fifo
  attr(px, "cast_fifo_path") <- cast_fifo
  attr(px, "input_fifo") <-
    processx::conn_create_file(input_fifo, read = FALSE, write = TRUE)
  attr(px, "cast_fifo") <-
    processx::conn_create_file(cast_fifo, read = TRUE, write = FALSE)

  wait_for_idle(px, timeout)

  # throw away the output of the startup code
  lines <- c(
    "Sys.setlocale('LC_ALL', 'en_US.UTF-8')",
    "options(cli.num_colors = 256)",
    if (!is.null(startup)) deparse(startup)
  )
  record_internal(lines, timeout, process = px)

  px
}

make_fifo <- function(path) {
  processx::run("mkfifo", path)
}

wait_for_idle <- function(px, timeout) {
  timeout <- as.double(timeout, units = "secs") * 1000
  output <- character()
  con <- attr(px, "cast_fifo")
  while (TRUE) {
    ret <- processx::poll(list(con), timeout)
    if (ret[[1]] == "timeout") {
      throw(new_error("Cannot start asciicast subprocess, timeout"))
    }
    line <- read_line(px)
    output[length(output) + 1L] <- line
    msg <- parse_line(line)
    if (msg$type == "rlib" && msg$value == "busy: 0") break
  }

  # there might we some more messages in the output, read those
  while (TRUE) {
    ret <- processx::poll(list(con), 0)
    if (ret[[1]] == "timeout") break
    line <-read_line(px)
    output[[length(output) + 1L]] <- line
  }
  output
}

read_line <- function(px) {
  con <- attr(px, "cast_fifo")
  processx::conn_read_lines(con, 1)
}

write_line <- function(px, line) {
  con <- attr(px, "input_fifo")
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
  if (typing_speed == 0) return(data)

  inp <- which(
    data$type == "rlib" & data$data == "type: input" &
    shift(data$type) == "o" & !grepl("#[!]\\s*$", shift(data$data))
  )

  data$data <- sub("\\s+#[!]\\s*$", "\r\n", data$data)

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
