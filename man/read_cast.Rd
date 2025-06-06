% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_cast}
\alias{read_cast}
\title{Import an asciicast from an asciicast JSON file}
\usage{
read_cast(json)
}
\arguments{
\item{json}{Path to JSON asciicast file, version 2:
\url{https://github.com/asciinema/asciinema/blob/master/doc/asciicast-v2.md}.
If a numeric id, then it is taken as a public \url{https://asciinema.org}
recording id, that is downloaded. It can also be a URL of private
\url{https://asciinema.org} link.}
}
\value{
\code{asciicast} object.
}
\description{
Import an asciicast from an asciicast JSON file
}
\examples{
\dontshow{if (interactive()) withAutoprint(\{ # examplesIf}
c1 <- read_cast("https://asciinema.org/a/uHQwIVpiZvu0Ioio8KYx6Uwlj.cast?dl=1")
play(c1)

c2 <- read_cast(258660)
play(c2)
\dontshow{\}) # examplesIf}
\dontshow{if (interactive()) withAutoprint(\{ # examplesIf}
c3 <- read_cast(system.file("examples", "hello.cast", package = "asciicast"))
play(c3)
\dontshow{\}) # examplesIf}
}
\seealso{
Other asciicast functions: 
\code{\link{asciicast-package}},
\code{\link{asciicast_start_process}()},
\code{\link{record}()},
\code{\link{write_json}()}
}
\concept{asciicast functions}
