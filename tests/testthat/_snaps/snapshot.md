# cast()

    Code
      cast(c("\033[32mabc\033[39m", "d"))
    Output
      <asciicast>
      <config>
        "version": 2,
        "command": "cat",
        "timestamp": 1746043110,
        "env": {
          "TERM": "xterm-256color",
          "SHELL": "/bin/zsh"
        },
        "height": 2,
        "rows": 2,
        "width": 3,
        "cols": 3
      
      <frames>
      # A tibble: 2 x 3
         time type  data                         
        <int> <chr> <chr>                        
      1     1 o     "\u001b[32mabc\u001b[39m\r\n"
      2     2 o     "d\r\n"                      

# capture_cast()

    Code
      capture_cast(cli::col_green("abc"))
    Output
      <asciicast>
      <config>
        "version": 2,
        "command": "cat",
        "timestamp": 1746043110,
        "env": {
          "TERM": "xterm-256color",
          "SHELL": "/bin/zsh"
        },
        "height": 2,
        "rows": 2,
        "width": 17,
        "cols": 17
      
      <frames>
      # A tibble: 2 x 3
         time type  data                             
        <int> <chr> <chr>                            
      1     1 o     "<cli_ansi_string>\r\n"          
      2     2 o     "[1] \u001b[32mabc\u001b[39m\r\n"

