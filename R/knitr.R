
#' Initialize the asciicast knitr engine
#'
#' Call this function in your Rmd file to enable creating asciinema
#' casts from code chunks.
#'
#' ## Limitations
#'
#' * `purl()` or setting the `purl = TRUE` chunk option, does not work
#'   properly, in that knitr thinks that asciicast chunks are not R code,
#'   so they will appear as comments. If you know how to fix this, please
#'   contact us.
#'
#' @param echo Whether to print the code of asciicast chunks.
#' @param same_process Whether to run all asciicast chunks _in the same_
#'   R process. To restart this R process, call `init_knitr_engine()`
#'   again.
#' @param echo_input Whether to echo the input in the asciicast recording.
#' @inheritParams asciicast_start_process
#'
#' @export
#' @family asciicast in Rmd
#' @section Examples:
#' Call this function from an Rmd chunk and then you can use the asciicast
#' knitr engine:
#' ````
#' ```{r setup, include = FALSE}
#' asciicast::init_knitr_engine()
#' ```
#' ````
#'
#' ````
#' ```{asciicast, cache = TRUE}`
#' #' Rows: 10
#' # This is an asciicast example
#' loadedNamespaces()
#' ```
#' ````

init_knitr_engine <- function(echo = FALSE, same_process = TRUE,
                              timeout = 10, startup = NULL,
                              record_env = NULL, echo_input = TRUE,
                              interactive = TRUE) {

  knitr::knit_engines$set("asciicast" = eng_asciicast)
  knitr::knit_engines$set("asciicastcpp11" = eng_asciicastcpp11)
  knitr::cache_engines$set("asciicast" = cache_eng_asciicast)

  output_type <- eng_asciicast_output_type()
  if (output_type == "widget") {
    deps <- htmlwidgets::getDependency("asciinema_player", "asciicast")
    knitr::knit_meta_add(deps)
  }

  default_echo <- knitr::opts_chunk$get("echo")
  attr(default_echo, "asciicast") <- echo
  knitr::opts_chunk$set(echo = default_echo)

  if (same_process) {
    proc <- asciicast_start_process(
      startup,
      timeout,
      record_env,
      interactive = interactive
    )
    oldproc <- .GlobalEnv$.knitr_asciicast_process
    if (!is.null(oldproc)) {
      try(close(oldproc$get_input_connection()), silent = TRUE)
      try(close(oldproc$get_output_connection()), silent = TRUE)
      try(close(attr(oldproc, "sock")), silent = TRUE)
      try(oldproc$kill(), silent = TRUE)
    }
    attr(proc, "echo") <- echo_input
    .GlobalEnv$.knitr_asciicast_process <- proc
  }
}

knitr_asciicast_options <- function(opts) {
  opts[grepl("^asciicast_", names(opts))]
}

eng_asciicast <- function(options) {
  # If 'echo' was specified directly, then attr(options$echo, "asciicast")
  # will be NULL, and we use the directly specified value.
  # otherwise we use the asciicast default, which is FALSE
  options$echo <- attr(options$echo, "asciicast") %||% options$echo

  withr::local_options(knitr_asciicast_options(options))

  if (!options$eval) {
    options$engine <- "r"
    return(knitr::engine_output(options, options$code, '', NULL))
  }

  cast_file <- tempfile()
  on.exit(unlink(cast_file), add = TRUE)
  writeLines(options$code %||% "", cast_file, useBytes = TRUE)

  if (!identical(options$echo, FALSE)) {
    options$code <- parse_header(options$code)$body
  }

  proc <- .GlobalEnv$.knitr_asciicast_process
  if (!is.null(proc) && !proc$is_alive()) {
    stop("asciicast subprocess crashed")
  }
  cast <- record(
    cast_file,
    process = proc,
    echo = options$asciicast_echo %||% attr(proc, "echo") %||% TRUE
  )

  if (options$cache > 0) cache_asciicast(cast, options$hash)

  options$engine <- "r"
  eng_asciicast_print(cast, options)
}

eng_asciicastcpp11 <- function(options) {
  withr::local_options(knitr_asciicast_options(options))
  if (options$eval) {
    # separate the headers
    ishead <- grepl("^#include", options$code)
    headers <- options$code[ishead]
    options$code <- options$code[!ishead]

    cast_file <- tempfile()
    cpp11_file <- tempfile(fileext = ".cc")
    on.exit(unlink(c(cast_file, cpp11_file)), add = TRUE)

    writeLines(c(
      "#include \"cpp11.hpp\"",
      "using namespace cpp11;",
      "namespace writable = cpp11::writable;",
      getOption("asciicast_cpp11_header"),
      headers,
      getOption("asciicast_cpp11_linkingto"),
      "[[cpp11::register]]",
      options$code
    ), cpp11_file, useBytes = TRUE)

    code <- deparse(bquote({
      cpp11::cpp_source(                                       # nocov start
        file = .(cpp11_file),
        env = .GlobalEnv,
        clean = .(options$clean %||% TRUE),
        quiet = .(options$quiet %||% FALSE),
        cxx_std = .(options$cxx_std %||% Sys.getenv("CXX_STD", "CXX11"))
      )                                                       # nocov end
    }))
    writeLines(c(code, "\n"), cast_file, useBytes = TRUE)

    proc <- .GlobalEnv$.knitr_asciicast_process
    if (!is.null(proc) && !proc$is_alive()) {
      stop("asciicast subprocess crashed")                    # nocov
    }

    cast <- record(cast_file, process = proc)

    # Change the engine to cpp so that code formatting works
    options$engine <- "cpp"
    knitr::engine_output(options, options$code, "")
  }
}

cache_eng_asciicast <- function(options) {
  options$echo <- attr(options$echo, "asciicast") %||% options$echo
  withr::local_options(knitr_asciicast_options(options))
  cast <- readRDS(paste0(options$hash, ".cast"))
  eng_asciicast_print(cast, options)
}

eng_asciicast_output_type <- function() {
  opt <- getOption("asciicast_knitr_output", "auto")
  if (!identical(opt, "auto")) return(opt)

  at <- getOption("asciicast_at")
  pkgdown <- identical(Sys.getenv("IN_PKGDOWN"), "true")
  if (pkgdown) {
    if (identical(at, "all")) {
      # animation
      "svg"
    } else {
      # screenshot
      "html"
    }

  } else {
    "svg"
  }
}

eng_asciicast_print <- function(cast, options) {
  output <- eng_asciicast_output_type()
  extra <- if (!options$include) {
    NULL
  } else if (output == "svg") {
    asciicast_knitr_svg(cast, options)
  } else if (output == "widget") {
    knitr::knit_print(asciinema_player(cast), options = options)
  } else {
    asciicast_knitr_html(cast, options)
  }

  knitr::engine_output(options, options$code, '', extra)
}

eng_write_html <- function(cast, path) {
  ocon <- file(path, open = "wb")

  prefix <- knitr::opts_chunk$get("comment")
  if (nchar(prefix) != 0) prefix <- paste0(prefix, " ")

  details <- getOption("asciicast_html_details", FALSE)
  write_html(
    cast,
    ocon,
    at = getOption("asciicast_at", "end"),
    prefix = prefix,
    details = details
  )
  close(ocon)
}

## Caching support. We cache both the cast and the SVG file as well,
## if the output is SVG
cache_asciicast <- function(cast, path) {
  message("(1) Saving ", paste0(path, ".cast"))
  saveRDS(cast, paste0(path, ".cast"))
  otype <- eng_asciicast_output_type()
  if (otype == "svg") {
    write_svg(cast, paste0(path, ".svg"))
  } else if (otype == "html") {
    eng_write_html(cast, paste0(path, ".html"))
  }
}

asciicast_knitr_svg <- function(cast, options) {
  filename <- file.path(
    knitr::opts_current$get("fig.path"),
    paste0(options$label, ".svg"))
  mkdirp(dirname(filename))

  ## This might be cached already. If not cached, we cache it now.
  cached <- paste0(options$hash, ".svg")
  if (options$cache > 0 && file.exists(cached)) {
    file.copy(cached, filename, overwrite = TRUE)
  } else {
    write_svg(cast, filename)
    if (options$cache > 0) file.copy(filename, cached)        # nocov
  }

  fig_proc <- knitr::opts_current$get("fig.process")
  if (!is.null(fig_proc)) filename <- fig_proc(filename)

  knitr::knit_hooks$get('plot')(filename, options)
}

asciicast_knitr_html <- function(cast, options) {
  cached <- paste0(options$hash, ".html")
  if (options$cache > 0 && file.exists(cached)) {
    html <- readLines(cached)                                 # nocov
  } else {
    if (options$cache > 0) {
      path <- cached
    } else {
      path <- tempfile("ascii-", fileext = ".html")
      on.exit(unlink(path), add = TRUE)
    }

    eng_write_html(cast, path)
    html <- readLines(path)
  }

  knitr::engine_output(
    knitr::opts_chunk$merge(list(results = "asis", echo = FALSE)),
    code = NULL,
    out = html
  )
}
