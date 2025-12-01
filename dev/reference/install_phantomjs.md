# Install PhantomJS

Download the zip package, unzip it, and copy the executable to a system
directory in which asciicast can look for the PhantomJS executable.

## Usage

``` r
install_phantomjs(
  version = "2.1.1",
  baseURL = "https://github.com/wch/webshot/releases/download/v0.3.1/",
  quiet = FALSE
)
```

## Arguments

- version:

  The version number of PhantomJS.

- baseURL:

  The base URL for the location of PhantomJS binaries for download. If
  the default download site is unavailable, you may specify an
  alternative mirror, such as
  `"https://bitbucket.org/ariya/phantomjs/downloads/"`.

- quiet:

  If `TRUE` suppress status messages and progress bar.

## Value

`NULL` (the executable is written to a system directory).

## Details

This function was designed primarily to help Windows users since it is
cumbersome to modify the `PATH` variable. Mac OS X users may install
PhantomJS via Homebrew. If you download the package from the PhantomJS
website instead, please make sure the executable can be found via the
`PATH` variable.

On Windows, the directory specified by the environment variable
`APPDATA` is used to store `phantomjs.exe`. On OS X, the directory
`~/Library/Application Support` is used. On other platforms (such as
Linux), the directory `~/bin` is used. If these directories are not
writable, the directory `PhantomJS` under the installation directory of
the asciicast package will be tried. If this directory still fails, you
will have to install PhantomJS by yourself.
