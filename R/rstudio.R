
is_rstudio <- function() {
  "rstudioapi" %in% loadedNamespaces() &&
    rstudioapi::isAvailable()
}

view_image_in_rstudio <- function(path) {
  html <- tempfile("asciicast-preview-", fileext = ".html")
  img <- paste0(
    tempfile("asciicast-preview-", fileext = ""),
    file_ext(path)
  )
  file.copy(path, img)

  cat(file = html, sprintf(
    "<html><body><img src=\"%s\" width=\"100%%\"></body></html>",
    basename(img)
  ))

  rstudioapi::viewer(html)
}
