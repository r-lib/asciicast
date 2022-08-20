
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Using ascii casts in GitHub README files

GitHub READMEs do not allow custom JavaScript code, so we cannot use
HTML widgets in them. But they do allow SVG images, which can also be
animated. This is file is an example README. See the source Rmd file.

First we need to initialize the asciicast engine, as usual:

    ```{r echo = FALSE, results = "hide"}
    asciicast::init_knitr_engine()
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

<img src="man/figures/README-/unnamed-chunk-3.svg" width="100%" />

## Proper ASCII casts

To use animated casts instead of screen shots, we need to set the `at`
option to `all`. We also set `end_wait` to wait five second before
restarting the animation. By default asciicast creates animated SVG
files:

    ```{asciicast, cache = TRUE}
    #' Rows: 10
    #' End_wait: 5
    #' At: all
    # This is an asciicast example
    loadedNamespaces()
    ```

<img src="man/figures/README-/unnamed-chunk-4.svg" width="100%" />

## ANSI colors

asciicast supports 256 ANSI colors, and ANSI support is automatically
enabled in the asciicast subprocess:

    ```{asciicast, cache = TRUE}
    cli::ansi_palette_show()
    ```

<img src="man/figures/README-/unnamed-chunk-5.svg" width="100%" />
