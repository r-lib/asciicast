
# nocov start

## This is from the webshot package, see https://github.com/wch/webshot

#' Install PhantomJS
#'
#' Download the zip package, unzip it, and copy the executable to a system
#' directory in which \pkg{asciicast} can look for the PhantomJS executable.
#'
#' This function was designed primarily to help Windows users since it is
#' cumbersome to modify the \code{PATH} variable. Mac OS X users may install
#' PhantomJS via Homebrew. If you download the package from the PhantomJS
#' website instead, please make sure the executable can be found via the
#' \code{PATH} variable.
#'
#' On Windows, the directory specified by the environment variable
#' \code{APPDATA} is used to store \file{phantomjs.exe}. On OS X, the directory
#' \file{~/Library/Application Support} is used. On other platforms (such as
#' Linux), the directory \file{~/bin} is used. If these directories are not
#' writable, the directory \file{PhantomJS} under the installation directory of
#' the \pkg{asciicast} package will be tried. If this directory still fails, you
#' will have to install PhantomJS by yourself.
#' @param version The version number of PhantomJS.
#' @param baseURL The base URL for the location of PhantomJS binaries for
#'   download. If the default download site is unavailable, you may specify an
#'   alternative mirror, such as
#'   \code{"https://bitbucket.org/ariya/phantomjs/downloads/"}.
#' @param quiet If `TRUE` suppress status messages and progress bar.
#' @return \code{NULL} (the executable is written to a system directory).
#' @export

install_phantomjs <- function(version = '2.1.1',
                              baseURL = 'https://github.com/wch/webshot/releases/download/v0.3.1/',
                              quiet = FALSE) {

  if (!grepl("/$", baseURL)) baseURL <- paste0(baseURL, "/")

  owd <- setwd(tempdir())
  on.exit(setwd(owd), add = TRUE)
  if (is_windows()) {
    zipfile <- sprintf('phantomjs-%s-windows.zip', version)
    utils::download.file(paste0(baseURL, zipfile), zipfile, mode = 'wb', quiet = quiet)
    utils::unzip(zipfile)
    zipdir <- sub('.zip$', '', zipfile)
    exec <- file.path(zipdir, 'bin', 'phantomjs.exe')
  } else if (is_macos()) {
    zipfile <- sprintf('phantomjs-%s-macosx.zip', version)
    utils::download.file(paste0(baseURL, zipfile), zipfile, mode = 'wb', quiet = quiet)
    utils::unzip(zipfile)
    zipdir <- sub('.zip$', '', zipfile)
    exec <- file.path(zipdir, 'bin', 'phantomjs')
    Sys.chmod(exec, '0755')  # chmod +x
  } else if (is_linux()) {
    zipfile <- sprintf(
      'phantomjs-%s-linux-%s.tar.bz2', version,
      if (grepl('64', Sys.info()[['machine']])) 'x86_64' else 'i686'
    )
    utils::download.file(paste0(baseURL, zipfile), zipfile, mode = 'wb', quiet = quiet)
    utils::untar(zipfile)
    zipdir <- sub('.tar.bz2$', '', zipfile)
    exec <- file.path(zipdir, 'bin', 'phantomjs')
    Sys.chmod(exec, '0755')  # chmod +x
  } else {
    # Unsupported platform, like Solaris
    if (!quiet) message("Sorry, this platform is not supported.")
    return(invisible())
  }
  success <- FALSE
  dirs <- phantom_paths()
  for (destdir in dirs) {
    dir.create(destdir, showWarnings = FALSE)
    success <- file.copy(exec, destdir, overwrite = TRUE)
    if (success) break
  }
  unlink(c(zipdir, zipfile), recursive = TRUE)
  if (!success) stop(
    'Unable to install PhantomJS to any of these dirs: ',
    paste(dirs, collapse = ', ')
  )
  if (!quiet) {
    message('phantomjs has been installed to ', normalizePath(destdir))
  }
  invisible()
}

# Possible locations of the PhantomJS executable
phantom_paths <- function() {
  if (is_windows()) {
    path <- Sys.getenv('APPDATA', '')
    path <- if (dir_exists(path)) file.path(path, 'PhantomJS')
  } else if (is_macos()) {
    path <- '~/Library/Application Support'
    path <- if (dir_exists(path)) file.path(path, 'PhantomJS')
  } else {
    path <- '~/bin'
  }
  path <- c(path, system.file('PhantomJS', package = .packageName))
  path
}

# Find PhantomJS from PATH, APPDATA, system.file('asciicast'), ~/bin, etc
find_phantom <- function() {

  for (d in phantom_paths()) {
    exec <- if (is_windows()) "phantomjs.exe" else "phantomjs"
    path <- file.path(d, exec)
    if (utils::file_test("-x", path)) break else path <- ""
  }

  if (path == "") {
    path <- Sys.which( "phantomjs" )
    if (path != "") return(path)
  }

  if (path == "") {
    # It would make the most sense to throw an error here. However, that would
    # cause problems with CRAN. The CRAN checking systems may not have phantomjs
    # and may not be capable of installing phantomjs (like on Solaris), and any
    # packages which use asciicast in their R CMD check (in examples or vignettes)
    # will get an ERROR. We'll issue a message and return NULL; other
    message(
      "PhantomJS not found. You can install it with asciicast::install_phantomjs(). ",
      "If it is installed, please make sure the phantomjs executable ",
      "can be found via the PATH variable."
    )
    return(NULL)
  }
  path.expand(path)
}

# nocov end
