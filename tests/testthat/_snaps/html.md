# create_markup_{fg,bg}

    Code
      create_markup_fg(4)
    Output
      $class
      [1] "ansi-color-4"
      
    Code
      create_markup_fg(12)
    Output
      $class
      [1] "ansi-color-12"
      
    Code
      create_markup_fg(c(1, 2, 3))
    Output
      $style
      [1] "color: #010203"
      

---

    Code
      create_markup_bg(4)
    Output
      $class
      [1] "ansi-bg-color-4"
      
    Code
      create_markup_bg(12)
    Output
      $class
      [1] "ansi-bg-color-12"
      
    Code
      create_markup_bg(c(1, 2, 3))
    Output
      $style
      [1] "background-color: #010203"
      

