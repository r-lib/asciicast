
record_internal <- function(lines, timeout, process,
                            incomplete_error = TRUE) {

  to <- as.double(timeout, units = "secs") * 1000

  ptr <- 1
  output <- character()
  con <- attr(process, "sock")

  send_next_command <- function() {
    while (TRUE) {
      if (!processx::conn_is_incomplete(con)) {
        # R exited unexpectedly while evaluating an expression
        return()                                                    # nocov
      }

      # if need more lines, but there is no more
      if (ptr > length(lines)) {
        if (incomplete_error) {
          throw(new_error("Incomplete asciicast expression"))
        } else {
          return()
        }
      }

      # pause?
      if (lines[ptr] == "#! --") {
        # Unlikely, there is at least a prompt usually?
        if (length(output) == 0) {
          ptr <<- ptr + 1
          next
        }
        ts <- parse_line(output[[length(output)]])$timestamp
        output <<- c(
          output,
          sprintf("[%f, \"rlib\", \"type: wait\"]", ts),
          sprintf("[%f, \"o\", \"\"]", ts)
        )
        ptr <<- ptr + 1
        next
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
      err <- new_error(
        "asciicast timeout after line ", linum,
        "\noutput:\n", paste(utils::tail(output), collapse = "\n"),
        "\nstdout:\n", px$read_output()
      )
      throw(err)
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
                            start_wait, end_wait,
                            record_env, startup, echo, speed, process,
                            interactive, locales, options,
                            incomplete_error) {

  px <- process

  if (is.null(px)) {
    px <- asciicast_start_process(
      startup,
      timeout,
      record_env,
      interactive,
      locales,
      options
    )
    on.exit(close(attr(px, "sock")), add = TRUE)
  }

  data <- record_internal(lines, timeout, px, incomplete_error)

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

  # Need to call this before adding the typing speed, because it moves
  # rows around
  data <- add_empty_wait(data, empty_wait)

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
#' @param locales Locales to set in the asciicast subprocess. Defaults
#'   to the current locales in the main R process. Specify a named character
#'   vector here to override some of the defaults. See also [get_locales()].
#' @param options Options to set in the subprocess, a named list.
#'   They are deparsed to code, and then the code setting them is
#'   executed in the subprocess. See [asciicast_options()] for the
#'   defaults. Supply a named list here to override the defaults or set
#'   additionsl ones. Passing large and/or complicated options here might
#'   not work, or might be slow.
#' @return The R process, a [processx::process] object.
#'
#' @family asciicast functions
#' @export
#' @examplesIf !asciicast:::is_rcmd_check()
#' # Use the same R process to record multiple casts
#' process <- asciicast_start_process()
#' script1 <- "a <- runif(10)\n"
#' script2 <- "a\n"
#' cast1 <- record(textConnection(script1), process = process)
#' cast2 <- record(textConnection(script2), process = process)
#' cast1
#' cast2

asciicast_start_process <- function(startup = NULL, timeout = 10,
                                    record_env = NULL, interactive = TRUE,
                                    locales = get_locales(),
                                    options = NULL) {

  sock <- processx::conn_create_unix_socket()
  sock_name <- processx::conn_file_name(sock)
  if (is_windows()) sock_name <- basename(sock_name) # nocovif !is_windows()

  env <- setup_env(record_env)
  env[["R_ASCIICAST_SOCKET"]] <- sock_name
  env[["R_ASCIICAST_INTERACTIVE"]] <- if (interactive) "true" else "false"
  px <- asciicast_start_process_internal(sock_name, env, interactive)
  attr(px, "sock") <- sock

  pr <- processx::poll(list(sock), 5000)[[1]]
  if (pr != "connect") {
    throw(new_error("R subprocess did not connect back"))
  }
  processx::conn_accept_unix_socket(sock)

  to <- as.double(timeout, units = "secs") * 1000
  wait_for(px, "^rlib$", "^busy: 0$", timeout = to)
  read_all(px)

  # throw away the output of the startup code
  set_locale_code <- paste0(
    "Sys.setlocale('", names(locales), "', '", locales, "')"
  )
  options <- modify_list(asciicast_options(), options)
  set_options_code <- paste0(
    "options(", names(options), " = ",
    map_chr(options, function(x) paste(deparse(x), collapse = "\n")),
    ")"
  )
  lines <- c(
    set_locale_code,
    set_options_code,
    if (!is.null(startup)) deparse(startup)
  )
  output <- record_internal(lines, timeout, process = px)

  if (!processx::conn_is_incomplete(sock)) {
    throw(new_error("asciicast process exited while running `startup`"))
  }

  px
}

asciicast_start_process_internal <- function(sock_name, env, interactive) {
  if (is_embedded()) {
    exec_path <- find_rem()
    px <- processx::process$new(
      exec_path,
      c(if (interactive) "-i", sock_name),
      env = env,
      stdout = "|",
      stderr = "2>&1"
      )

  } else {
    r <- file.path(R.home(component="bin"), "R")
    px <- processx::process$new(
      # TODO: find R executable, like in callr
      r,
      c(
        "-q",
        "--vanilla",
        "--no-restore",
        "--no-save",
        "--no-readline"
      ),
      env = env,
      stdin = "|", stdout = "|", stderr = "2>&1"
    )
    client <- find_client()
    cmd <- sprintf("dyn.load('%s')\n", client)
    px$write_input(cmd)
  }

  px
}

setup_env <- function(extra = NULL) {
  env <- Sys.getenv()
  env["ASCIICAST"] <- "true"
  env["R_CLI_HYPERLINK_MODE"] <- "posix"
  if (is_windows()) {
    env["PATH"] <- paste0(R.home("bin"), ";", Sys.getenv("PATH")) # nocovif !is_windows()
  }
  env[names(extra)] <- extra
  env <- na_omit(env)
}

get_embedded <- function() {
  exec_name <- if (.Platform$OS.type == "windows") "rem.exe" else "rem"
  exec_path <- system.file("src", exec_name, package = "asciicast")
  if (exec_path == "") {
    exec_path <- system.file(
      "bin",
      .Platform$r_arch,
      exec_name,
      package = "asciicast"
    )
  }
  exec_path
}

has_embedded <- function() {
  get_embedded() != ""
}

is_embedded <- function() {
  default <- if (has_embedded()) "true" else "false"
  tolower(Sys.getenv("R_ASCIICAST_EMBEDDED", default)) == "true"
}

find_rem <- function() {
  exec_path <- get_embedded()
  if (exec_path == "") {
    exec_name <- if (.Platform$OS.type == "windows") "rem.exe" else "rem"
    throw(new_error("Cannot find embedded R executable ", exec_name))
  }
  exec_path
}

find_client <- function() {
  lib_name <- if (.Platform$OS.type == "windows") {
    "asciicastclient.dll"
  } else {
    "asciicastclient.so"
  }
  lib_path <- system.file("src", lib_name, package = "asciicast")
  if (lib_path == "") {
    lib_path <- system.file(
      "bin",
      .Platform$r_arch,
      lib_name,
      package = "asciicast"
    )
  }
  if (lib_path == "") {
    throw(new_error("Cannot find embedded R executable ", lib_name))
  }
  lib_path
}

write_line <- function(px, line) {
  con <- attr(px, "sock")
  line <- paste0(line, "\n")
  leftover <- processx::conn_write(con, charToRaw(line))
  if (length(leftover) > 0) {
    throw(new_error("Cannot send input, buffer is full, line too long?"))
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
    data$type == "rlib" & data$data %in% c("type: input", "type: wait") &
    shift(data$type) == "o" & shift(data$data) %in% c("\r\n", "")
  )

  if (length(empty) == 0) return(data)

  # If the next entry is a prompt, then move the wait after the prompt
  for (eidx in empty) {
    skip <- 0L
    while (TRUE && eidx + 2L + skip <= nrow(data)) {
      typ <- data$type[eidx + 2L + skip]
      typ1 <- data$type[eidx + 2L + skip + 1L]
      dat <- data$data[eidx + 2L + skip]
      dat1 <- data$data[eidx + 2L + skip + 1L]
      if (typ == "rlib" && dat == "type: read") {
        skip <- skip + 1L
      } else if (typ == "rlib" && dat == "type: prompt" && typ1 == "o") {
        skip <- skip + 2L
      } else {
        break
      }
    }
    if (skip > 0) {
      data$time[eidx:(eidx+1)] <- data$time[eidx + 1 + skip]
      data[eidx:(eidx + 1 + skip), ] <-
        data[c((eidx + 2):(eidx + 1 + skip), eidx, eidx + 1L), ]
    }
  }

  # we might have shifted them, so calculate again
  empty <- which(
    data$type == "rlib" & data$data %in% c("type: input", "type: wait") &
    shift(data$type) == "o" & shift(data$data) %in% c("\r\n", "")
  )

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

#' Helper function to query locales as a named character vector.
#'
#' @return Named character vector with entries:
#' * `LC_COLLATE`, `LC_CTYPE`, `LC_MONETARY`, `LC_NUMERIC` and `LC_TIME`.
#'
#' @export

get_locales <- function() {
  lcls <- c(
    "LC_COLLATE",
    "LC_CTYPE",
    "LC_MONETARY",
    "LC_NUMERIC",
    "LC_TIME"
  )

  structure(
    map_chr(lcls, Sys.getlocale),
    names = lcls
  )
}

#' Default options to set in the asciicast subprocess.
#'
#' @return Named list.
#'
#' @export
#' @examples
#' asciicast_options()

asciicast_options <- function() {
  list(
    cli.num_colors = 256,
    cli.dynamic = TRUE,
    cli.ansi = TRUE
  )
}
