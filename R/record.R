
record_commands <- function(lines, timeout = 10) {
  px <- processx::process$new("R", "-q", pty = TRUE)
  on.exit(px$kill(), add = TRUE)

  ready <- px$poll_io(5000)
  if (!px$is_alive() || ready["output"] != "ready") {
    throw("R subprocess is not ready after 5s")
  }
  

  start <- Sys.time()
  output <- list(list(0, px$read_output()))
  next_line <- 1L
  
  poll_with_timeout <- function() {
    ready <- px$poll_io(timeout * 1000)
    if (ready["process"] == "ready") stop("R subprocess crashed")
    if (ready["output"] != "ready") stop("R subprocess frozen or slow")
  }
  
  read_output <- function() {
    out <- ""
    while (TRUE) {
      poll_with_timeout()
      newout <- px$read_output()
      print(newout)
      output <<- append(output, list(list(Sys.time() - start, newout)))
      out <- paste0(out, newout)
      if (grepl("READY\r?\n", out)) {
        cat("GOT:\n", out)
        return()
      }
    }
  }

  next_expression <- function() {
    end <- next_line
    while (end <= length(lines) && ! is_complete(lines[next_line:end])) {
      end <- end + 1L
    }
    if (end > length(lines)) {
      stop("Incomplete expression at end of file, from line ", next_line)
    }

    expr <- paste0(lines[next_line:end], "\n", collapse = "")
    next_line <<- end + 1L
    expr
  }

  while (next_line <= length(lines)) {
    expr <- next_expression()
    px$write_input(paste0(expr, "cat('READY\n')\n"))
    cat("RUNNING\n", expr)
    output <- append(output, list(list(Sys.time() - start, expr)))
    read_output()
  }

  tibble::tibble(
    time = as.double(vapply(output, "[[", double(1), 1), units = "secs"),
    type = "o",
    data = vapply(output, "[[", character(1), 2))
}

is_complete <- function(x) {
  err <- NULL
  tryCatch(parse(text = x), error = function(e) err <<- e)
  if (is.null(err)) return(TRUE)
  exp <- tryCatch(parse(text = "1+"), error = function(e) e$message)
  exp1 <- strsplit(exp, "\n")[[1]][[1]]
  msg <- sub("^.*:\\s*([^:]+)$",  "\\1", exp1, perl = TRUE)
  ! grepl(msg, conditionMessage(err), fixed = TRUE)
}
