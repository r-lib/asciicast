
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Using ascii casts in GitHub README files

GitHub READMEs do not allow custom JavaScript code, so we cannot use
HTML widgets in them. But they do allow SVG images, which can also be
animated. This is file is an example README. See the source Rmd file.

First we need to initialize the asciicast engine, as usual:

    ```{r echo = FALSE, results = "hide"}
    asciicast::init_knitr_engine()
    ```

Next we need to set an option to use SVG files instead of the asciinema
player HTML widget.

    ```{r, include = FALSE}
    options(asciicast_knitr_svg = TRUE)
    ```

Now we are ready to include casts. The current default is to create a
snapshot of the screen after the code has run:

## Still screenshots

To include a snapshot instead of an animation, the `at` option must be
set to `"end"`, but that is the default currently:

    ```{asciicast, cache = TRUE}
    # This is an asciicast example
    loadedNamespaces()
    ```

<img src="man/figures/README-/unnamed-chunk-4.svg" width="100%" />

## Proper ASCII casts

To use animated casts instead of screen shots, we need to set the `at`
option to `NULL`. (It is not currently possible to do this from the
header.) We also set `end_wait` to wait three second before restarting
the animation. By default asciicast creates animated SVG files:

    ```{asciicast, cache = TRUE, R.options = list(asciicast_at = NULL)}
    #' Rows: 10
    #' End_wait: 3
    # This is an asciicast example
    loadedNamespaces()
    ```

<img src="man/figures/README-/unnamed-chunk-5.svg" width="100%" />

## ANSI colors

asciicast supports 256 ANSI colors, and ANSI support is automatically
enabled in the asciicast subprocess:

    ```{asciicast, cache = TRUE}
    cli::ansi_palette_show()
    ```

<img src="man/figures/README-/unnamed-chunk-6.svg" width="100%" />
