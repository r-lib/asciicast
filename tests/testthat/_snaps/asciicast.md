# record

    Code
      cast$output$data
    Output
       [1] "type: prompt"                "> "                         
       [3] "print(\"Hello world!\")\r\n" "type: input"                
       [5] "print(\"Hello world!\")\r\n" "busy: 1"                    
       [7] "type: stdout"                "[1]"                        
       [9] "type: stdout"                " \"Hello world!\""          
      [11] "type: stdout"                "\r\n"                       
      [13] "busy: 0"                     "type: read"                 
      [15] "type: wait"                  ""                           

# env vars in header

    Code
      cast$output$data
    Output
       [1] "Sys.getenv('FOO')\r\n"                 
       [2] "busy: 1"                               
       [3] "type: stdout"                          
       [4] "[1]"                                   
       [5] "type: stdout"                          
       [6] " \"bar\""                              
       [7] "type: stdout"                          
       [8] "\r\n"                                  
       [9] "busy: 0"                               
      [10] "type: read"                            
      [11] "Sys.getenv('SHELL', NA_character_)\r\n"
      [12] "busy: 1"                               
      [13] "type: stdout"                          
      [14] "[1]"                                   
      [15] "type: stdout"                          
      [16] " NA"                                   
      [17] "type: stdout"                          
      [18] "\r\n"                                  
      [19] "busy: 0"                               
      [20] "type: read"                            
      [21] "type: wait"                            
      [22] ""                                      

# startup option

    Code
      cast$output$data
    Output
       [1] "foo\r\n"      "busy: 1"      "type: stdout" "[1]"          "type: stdout"
       [6] " 112"         "type: stdout" "\r\n"         "busy: 0"      "type: read"  
      [11] "type: wait"   ""            

# startup in header

    Code
      cast$output$data
    Output
       [1] "foo\r\n"      "busy: 1"      "type: stdout" "[1]"          "type: stdout"
       [6] " 112"         "type: stdout" "\r\n"         "busy: 0"      "type: read"  
      [11] "type: wait"   ""            

# print.asciicast

    Code
      cast
    Output
      <asciicast>
      <config>
        "version": 2,
        "command": "R -q",
        "timestamp": 1656941462,
        "env": {
          "TERM": "xterm-256color",
          "SHELL": "/bin/zsh"
        },
        "height": 24,
        "rows": 24,
        "width": 80,
        "cols": 80
      
      <frames>
      # A tibble: 35 x 3
          time type  data           
         <dbl> <chr> <chr>          
       1  0.01 rlib  "type: prompt" 
       2  0.02 o     "> "           
       3  0.03 i     "# comment\r\n"
       4  0.04 rlib  "type: input"  
       5  0.05 o     "#"            
       6  0.06 o     " "            
       7  0.07 o     "c"            
       8  0.08 o     "o"            
       9  0.09 o     "m"            
      10  0.1  o     "m"            
      # ... with 25 more rows

