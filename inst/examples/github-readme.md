
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

Now we are ready to include casts. By default asciicast creates animated
SVG files:

    ```{asciicast, cache = TRUE}
    #' Rows: 10
    # This is an asciicast example
    loadedNamespaces()
    ```

<img src="man/figures/README-/unnamed-chunk-4.svg" width="100%" />

## Still screenshots

To include a snapshot instead of an animation, use `At: end` in the
asciicast header.

    ```{asciicast, cache = TRUE}
    #' Rows: 10
    #' At: end
    # This is an asciicast example
    loadedNamespaces()
    ```

<img src="man/figures/README-/unnamed-chunk-5.svg" width="100%" />
