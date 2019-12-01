
env_file <- NULL

.onLoad <- function(libname, pkgname) {
  env <- new.env(parent = emptyenv())
  env$`__asciicast_data__` <- new.env(parent = baseenv())

  client_file <- system.file("client.R", package = "asciicast")
  if (client_file == "") stop("Cannot find client R file")

  source(
    client_file, local = env$`__asciicast_data__`,
    keep.source = FALSE)

  arch <- .Platform$r_arch
  ext <- .Platform$dynlib.ext
  sofile <- system.file(
    "libs", arch, paste0("client", ext),
    package = "processx")

  # Maybe not multi-arch build on a multi-arch system?
  # Can this happent at all?
  if (sofile == "") {
    sofile <- system.file(
      "libs", paste0("client", ext),
      package = "processx")
  }

  # Try this as well, this is for devtools/pkgload
  if (sofile == "") {
    sofile <- system.file(
      "src", paste0("client", ext),
      package = "processx")
  }

  # stop() here and not throw(), because this function should be standalone
  if (sofile == "") stop("Cannot find client file")

  env$`__asciicast_data__`$sofile <- sofile

  env_file <<- tempfile()
  saveRDS(env, file = env_file, version = 2, compress = FALSE)

  lazyrmd$onload_hook(
    local = FALSE,
    ci = function() is_recording_supported(),
    cran = "no-code"
  )

  invisible()
}
