# write_svg errors

    Code
      write_svg(cast, svg, theme = "foobarxx")
    Condition
      Error:
      ! Unknown theme: foobarxx

---

    Code
      check_svg_support()
    Condition
      Error:
      ! Writing SVG files needs a more recent Node library.
      i See the documentation of the V8 package: <https://github.com/jeroen/v8#readme>.

