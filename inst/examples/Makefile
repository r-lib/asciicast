
all: asciicast.cast dplyr.cast errors.cast hello.cast github-readme.md

%.cast: %.R
	R -q -e 'asciicast::write_json(asciicast::record("$<"), "$@")'

%.md: %.Rmd
	R -q -e 'rmarkdown::render("$<")'
